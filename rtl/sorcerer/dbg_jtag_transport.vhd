-- Project Sorcerer - RISC-V core
--
-- JTAG Transport Module
--

--
-- Copyright (C) 2017
-- Alexey Shistko     alexey@kudeyar.com
-- Andrei Safronov    andrei@kudeyar.com
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_UNSIGNED.all;
use IEEE.STD_LOGIC_ARITH.all;
use work.utils.all;
use work.iface.all;

entity dbg_jtag_transport is
	generic
	(
	  DATA_WIDTH: natural range 32 to 128 := 32;
		ADDRESS_WIDTH: natural range 32 to 128 := 32;
		SERIAL_WIDTH: natural range 8 to 128 := 32;
		INT_BITS: natural range 1 to 32 := 1;
		SER_BITS: natural range 1 to 8 := 1;
		VERSION: natural range 0 to 15 := 1;
		PART_NUMBER: natural range 0 to 65535 := 1;
		MANUF_ID: natural range 0 to 2047 := 1;
		CRC_LENGTH: natural range 8 to 32 := 8  -- 8, 16 or 32 supported values
	);
  port
  (
	  clk: in std_logic;
		reset: in std_logic; 

		dbg_reset: out std_logic;    -- Debugger reset output
		dbg_ndreset: out std_logic;  -- Debugger non-halt reset	mode
		
		-- Interface to Bus Bridge
		bus_tck: out std_logic;		-- JTAG clock
		bus_address: out std_logic_vector(ADDRESS_WIDTH-1 downto 0);
		bus_size: out std_logic_vector(2 downto 0);
		bus_strobe: out std_logic;
		bus_writedata: out std_logic_vector(DATA_WIDTH-1 downto 0);
		bus_write: out std_logic;
		bus_readdata: in std_logic_vector(DATA_WIDTH-1 downto 0);
		bus_ready: in std_logic;
		
    -- Signals from Virtual JTAG instance
    tck: in std_logic;
	  tdi: in std_logic;
    capture_dr: in std_logic;
	  e1dr: in std_logic;
	  e2dr: in std_logic;
	  pdr: in std_logic;
	  shift_dr: in std_logic;
	  update_dr: in std_logic;
	  update_ir: in std_logic;
	  cir: in std_logic;
	  tms: in std_logic;
		IR: in std_logic_vector(4 downto 0); -- IR command register	
    tdo: out std_logic
	);	
end dbg_jtag_transport;

architecture behaviour of dbg_jtag_transport is	
  constant DRSHIFT_WIDTH: natural := imax(DATA_WIDTH+2, 48);

  subtype JTAG_reg_address is std_logic_vector(4 downto 0);
	
	constant JTAG_IDCODE: JTAG_reg_address := "00001";         -- JTAG recommends this encoding
	constant JTAG_SAMPLE: JTAG_reg_address := "00010";         -- JTAG requires this instruction
	constant JTAG_PRELOAD: JTAG_reg_address := "00011";        -- JTAG requires this instruction
	constant JTAG_EXTEST: JTAG_reg_address :=  "00100";	       -- JTAG requires this instruction
	constant JTAG_INIT_SETUP_CLAMP: JTAG_reg_address := "00100";  -- JTAG recommends this instruction
	constant JTAG_CLAMP: JTAG_reg_address := 	"00101";	       -- JTAG recommends this instruction
	constant JTAG_CLAMP_HOLD: JTAG_reg_address := "00110";	   -- JTAG recommends this instruction
	constant JTAG_CLAMP_RELEASE: JTAG_reg_address := "00111";  -- JTAG recommends this instruction
	constant JTAG_HIGHZ: JTAG_reg_address := "01000";		       -- JTAG recommends this instruction
	constant JTAG_IC_RESET: JTAG_reg_address := "01001";	     -- JTAG recommends this instruction
	constant JTAG_TMP_STATUS: JTAG_reg_address := "01010";	   -- JTAG recommends this instruction
	constant JTAG_INIT_SETUP: JTAG_reg_address := "01011";     -- JTAG recommends this instruction
	constant JTAG_INIT_RUN: JTAG_reg_address := "01100";		   -- JTAG recommends this instruction
	constant JTAG_DTM_Control: JTAG_reg_address := "10000";    -- DTM Control
	constant JTAG_DTM_Authentication_Data: JTAG_reg_address := "10001";  -- DTM Authentication
	constant JTAG_Bus_Control: JTAG_reg_address := "10010";	   -- For bus access
	constant JTAG_Bus_Address: JTAG_reg_address := "10011";	   -- For bus access
	constant JTAG_Bus_Data: JTAG_reg_address := "10100";		   -- For bus access
	constant JTAG_Status: JTAG_reg_address :=	"10101";         -- For interrupts/serial
	constant JTAG_Status_Control: JTAG_reg_address := "10110"; -- For interrupts/serial
	constant JTAG_Serial_Data: JTAG_reg_address :=	"10111";	 -- For serial 
	constant JTAG_IN_CRC: JTAG_reg_address :=	"11000";         -- For input data from JTAG
	constant JTAG_OUT_CRC: JTAG_reg_address :=	"11001";       -- For output data to JTAG	
	constant JTAG_BYPASS: JTAG_reg_address := "11111";			   -- JTAG requires this encoding	
	
	type dtmcontrol_type is record
		abussize: std_logic_vector(7 downto 0);  -- Width of the address bus in bits.
		access128: std_logic;										 -- 1 when 128-bit bus accesses are supported.
		access64: std_logic;										 -- 1 when 64-bit bus accesses are supported.
		access32: std_logic;										 -- 1 when 32-bit bus accesses are supported.
		access16: std_logic;										 -- 1 when 16-bit bus accesses are supported.
		access8: std_logic;										   -- 1 when 32-bit bus accesses are supported.
		intbits: std_logic_vector(7 downto 0);  -- The width of the internal interrupt state is intbits + 1.
		authenticated: std_logic;								 -- 0 when authentication is required before using the DTM. 
						                                 -- 1 when the authentication check has passed.
		authbusy: std_logic;                     -- While 1, writes to authdata may be ignored or may result in authentication failing.
    authtype: std_logic_vector(1 downto 0);  -- Defnes the kind of authentication required to use this DTM.		
	  ndreset: std_logic;											 -- Every time this bit is written as 1, it triggers a full reset of the non-debug logic on the platform.
		fullreset: std_logic;										 -- Every time this bit is written as 1, it triggers a full reset of the platform,
	end record;
	
	constant SERIAL_NOT_SUPPORTED: std_logic_vector(1 downto 0) := "00";  -- Serial not supported
	constant SERIAL_32BITS: std_logic_vector(1 downto 0) := "01";					-- Serial interface 0 is supported and 32 bits wide.
	constant SERIAL_64BITS: std_logic_vector(1 downto 0) := "10";					-- Serial interface 0 is supported and 64 bits wide.
	constant SERIAL_128BITS: std_logic_vector(1 downto 0) := "01";			  -- Serial interface 0 is supported and 128 bits wide.
	
	type serialinfo_type is record
		serial7: std_logic_vector(1 downto 0);
		serial6: std_logic_vector(1 downto 0);
		serial5: std_logic_vector(1 downto 0);
		serial4: std_logic_vector(1 downto 0); 
		serial3: std_logic_vector(1 downto 0); 
		serial2: std_logic_vector(1 downto 0); 
		serial1: std_logic_vector(1 downto 0); 
		serial0: std_logic_vector(1 downto 0);
	end record;

	constant ERROR_NONE: std_logic_vector(1 downto 0) := "00";     -- No error has been seen.
	constant ERROR_TIMEOUT: std_logic_vector(1 downto 0) := "01";  -- Timeout error has been seen.
	constant ERROR_OTHER: std_logic_vector(1 downto 0) := "10";    -- Some other bus error has been seen. 
	
	-- Encoding hiabits
  -- 0  min(7, abussize - 1)
  -- 1  min(11, abussize - 1)
  -- 2  min(15, abussize - 1)
  -- 3  min(23, abussize - 1)
  -- 4  min(31, abussize - 1)
  -- 5  min(63, abussize - 1)
  -- 6  min(127, abussize - 1)
	
	type jbusc_type is record
		error: std_logic_vector(1 downto 0);
		dbits: std_logic_vector(4 downto 0);  -- Set hidbits to (dbits+1)*4-1.
		abits: std_logic_vector(2 downto 0); 	-- Set hiabits 
		size: std_logic_vector(2 downto 0);		-- Set size and loabits to 2**size * 8.
	end record;	
	
	type jaddress_type is record
		update: std_logic_vector(ADDRESS_WIDTH-1 downto 0);  -- Update address[hiabits:loabits] with the value in update.
		autoincrement: std_logic;  -- When set, increment address by size=8 after every scan of jdata.
--		read: std_logic;  -- Set this bit to perform a read at the updated address.
	end record;	
	
	type jdata_type is record
    data: std_logic_vector(ADDRESS_WIDTH-1 downto 0);  
		write: std_logic;  -- Set this bit to write data to the current address.
		read: std_logic;	 -- Set this bit to perform a read at the (possibly post-incremented) address.
  end record;

	type jstatus_type is record
		serial: std_logic_vector(SER_BITS-1 downto 0);     -- Bit N is 1 when serial port N is ready for the debugger to send data to it.
		interrupt: std_logic_vector(INT_BITS-1 downto 0);  -- Contains the intbits lower bits of the internal interrupt state. 
		                                                   -- Scanning this register clears the entire internal interrupt state.
	end record;	

	type jstatsc_type is record
		serscan: std_logic_vector(1 downto 0);		-- Select the number of serial ports that show up in jstatus.
		intscan: std_logic_vector(1 downto 0); 	  -- Select the number of interrupt status bits that show up in jstatus.
		serselect: std_logic_vector(2 downto 0);  -- Select which serial port jserial accesses.
	end record;	

	type jserial_type is record
		data: std_logic_vector(SERIAL_WIDTH-1 downto 0);
		write: std_logic; -- Set this bit to write data to the debugger-to-core queue.
	end record;	

	type tap_regs_type is record
		DR_Shift: std_logic_vector(DRSHIFT_WIDTH-1 downto 0);
		dtmcontrol: dtmcontrol_type;
		serinfo: serialinfo_type;
		jbusc: jbusc_type;
		jaddress: jaddress_type;
		jdata: jdata_type;
		jstatc: jstatsc_type;
		jserial: jserial_type;
		bus_strobe: std_logic;
	end record;	

	signal r, next_r: tap_regs_type;

	signal crc_in_enable: std_logic;
	signal crc_in_sclr: std_logic;
	signal crc_in_out: std_logic_vector(31 downto 0);
	
	signal crc_out_enable: std_logic;
	signal crc_out_sclr: std_logic;
	signal crc_out_out: std_logic_vector(31 downto 0);	

	signal serial: std_logic_vector(SER_BITS-1 downto 0) := (others => '0');
	signal interrupt: std_logic_vector(INT_BITS-1 downto 0) := (others => '0');

	signal reset_sync1, reset_sync2, reset_syncq : std_logic;
	signal ndreset_sync1, ndreset_sync2, ndreset_syncq : std_logic;
	signal fullreset: std_logic;
	signal ndreset: std_logic;
begin
	crc_in: dbg_crc
	generic map
	(
	  LENGTH => CRC_LENGTH
	)
	port map
	(
	  clk => tck,
		reset => reset,
		data => tdi,
		enable => crc_in_enable,
		shift => '0',
		sclr => crc_in_sclr,
		crc_out => crc_in_out
	);
	
	crc_out: dbg_crc
	generic map
	(
	  LENGTH => CRC_LENGTH
	)
	port map
	(
	  clk => tck,
		reset => reset,
		data => tdi,
		enable => crc_out_enable,
		shift => '0',
		sclr => crc_out_sclr,
		crc_out => crc_out_out
	);	
	
	next_state_proc: process(r,
	    IR, bus_ready, serial, interrupt,
	    tdi, capture_dr, shift_dr, update_dr)
	  variable v: Tap_regs_type;
		variable d7: std_logic_vector(6 downto 0);
		variable d13: std_logic_vector(12 downto 0);
		variable d16: std_logic_vector(15 downto 0);
		variable d32: std_logic_vector(31 downto 0);
		variable crc_in_en: std_logic;
		variable crc_out_en: std_logic;
		
		procedure ShiftDR is
		begin
			if shift_dr = '1' then
			 	v.DR_Shift := tdi & r.DR_Shift(r.DR_Shift'high downto 1);
			end if;	
			tdo <= r.DR_Shift(0);
		end procedure ShiftDR; 

		procedure CaptureDR(val: std_logic_vector) is
		begin
			if capture_dr = '1' then
				v.DR_Shift(val'high downto 0) := val;
			end if;
		end procedure CaptureDR;	
		
		procedure UpdateData(dsize: integer) is
		begin
			v.jdata.data := (others => '0');
			v.jdata.data(dsize-1 downto 0) := r.DR_Shift(r.DR_Shift'high downto r.DR_Shift'high-dsize+1);
			v.jdata.write := r.DR_Shift(r.DR_Shift'high-dsize);
			v.jdata.read := r.DR_Shift(r.DR_Shift'high-dsize-1);
		end procedure UpdateData;	
		
		procedure IncrementAddress is
		  variable inc: std_logic_vector(ADDRESS_WIDTH-1 downto 0);
		begin										
			inc := (others => '0');
			inc(CONV_INTEGER(r.jbusc.size)) := '1';
			v.jaddress.update := r.jaddress.update + inc;
		end procedure IncrementAddress;	
	begin
		v := r;
		v.bus_strobe := '0';
    crc_in_sclr <= '0';
		crc_out_sclr <= '0';
		crc_in_en := '0';
		crc_out_en := '0';
--    v.DR_Shift := (others => '0');
    ShiftDR;
		case IR is
      when JTAG_IDCODE =>
			  CaptureDR(CONV_STD_LOGIC_VECTOR(VERSION, 4) &
				  CONV_STD_LOGIC_VECTOR(PART_NUMBER, 16) &
					CONV_STD_LOGIC_VECTOR(MANUF_ID, 11) & '1');
      when JTAG_SAMPLE =>
			  tdo <= tdi;
      when JTAG_PRELOAD =>
			  tdo <= tdi;
      when JTAG_EXTEST =>
			  tdo <= tdi;
      when JTAG_CLAMP =>
			  tdo <= tdi;
      when JTAG_CLAMP_HOLD =>
			  tdo <= tdi;
      when JTAG_CLAMP_RELEASE =>
			  tdo <= tdi;
      when JTAG_HIGHZ =>
			  tdo <= tdi;
      when JTAG_IC_RESET =>
			  tdo <= tdi;
      when JTAG_TMP_STATUS =>
			  tdo <= tdi;
      when JTAG_INIT_SETUP =>
			  tdo <= tdi;
      when JTAG_INIT_RUN =>
			  tdo <= tdi;
      when JTAG_DTM_Control =>
			  CaptureDR(
			    r.serinfo.serial7 & r.serinfo.serial6 &
					r.serinfo.serial5 & r.serinfo.serial4 & r.serinfo.serial3 &
					r.serinfo.serial2 & r.serinfo.serial1 & r.serinfo.serial0 &
          r.dtmcontrol.abussize & "00" &
				  r.dtmcontrol.access128 & r.dtmcontrol.access64 &
				  r.dtmcontrol.access32 & r.dtmcontrol.access16 & r.dtmcontrol.access8 &
					r.dtmcontrol.intbits & "00" & r.dtmcontrol.authenticated &
					r.dtmcontrol.authbusy & r.dtmcontrol.authtype & '0' & '0');
				if update_dr = '1' then
					d16 := r.DR_Shift(r.DR_Shift'high downto r.DR_Shift'high-15);
					d32 := r.DR_Shift(r.DR_Shift'high-16 downto r.DR_Shift'high-47);
					v.serinfo.serial7 := d16(15 downto 14);
					v.serinfo.serial6 := d16(13 downto 12);
					v.serinfo.serial5 := d16(11 downto 10);
					v.serinfo.serial4 := d16(9 downto 8);
					v.serinfo.serial3 := d16(7 downto 6);
					v.serinfo.serial2 := d16(5 downto 4);
					v.serinfo.serial1 := d16(3 downto 2);
					v.serinfo.serial0 := d16(1 downto 0);
					v.dtmcontrol.ndreset := r.dtmcontrol.ndreset xor d32(1);
					v.dtmcontrol.fullreset := d32(0);
				end if;	
				crc_in_en := '1';
				crc_out_en := '1';
      when JTAG_DTM_Authentication_Data =>
			  tdo <= tdi;
      when JTAG_Bus_Control =>
			  CaptureDR(r.jbusc.error & r.jbusc.dbits & r.jbusc.abits & r.jbusc.size);
				if update_dr = '1' and bus_ready = '1' then
				  d13 := r.DR_Shift(r.DR_Shift'high downto r.DR_Shift'high-12);
					v.jbusc.error := d13(12 downto 11);
					v.jbusc.dbits := d13(10 downto 6);
					v.jbusc.abits := d13(5 downto 3);
					v.jbusc.size := d13(2 downto 0);
				end if;
				crc_in_en := '1';
				crc_out_en := '1';				
      when JTAG_Bus_Address =>
			  -- TODO use hiabits & loabits
			  CaptureDR(r.jaddress.update & r.jaddress.autoincrement & (not bus_ready));
				if update_dr = '1' and bus_ready = '1' then
					v.jaddress.update := r.DR_Shift(r.DR_Shift'high downto r.DR_Shift'high-ADDRESS_WIDTH+1);
					v.jaddress.autoincrement := r.DR_Shift(r.DR_Shift'high-ADDRESS_WIDTH);
					v.jdata.read := r.DR_Shift(r.DR_Shift'high-ADDRESS_WIDTH-1);
					if v.jdata.read = '1' and bus_ready = '1' then
					  v.bus_strobe := '1';
					end if;
					v.jdata.write := '0';
				end if;	
				crc_in_en := '1';
				crc_out_en := '1';				
      when JTAG_Bus_Data =>
				case r.jbusc.dbits is
					when "00001" =>  -- 8	
					  CaptureDR(bus_readdata(7 downto 0) & bus_ready & (not bus_ready));
					when "00011" =>  -- 16 
					  if DATA_WIDTH >= 16 then
							CaptureDR(bus_readdata(15 downto 0) & bus_ready & (not bus_ready));
						end if;	
					when "00111" =>  -- 32
					  if DATA_WIDTH >= 32 then
							CaptureDR(bus_readdata(31 downto 0) & bus_ready & (not bus_ready));
						end if;						
					when "01111" =>  -- 64
					  if DATA_WIDTH >= 64 then
						  CaptureDR(bus_readdata(63 downto 0) & bus_ready & (not bus_ready));
						end if;						
					when "11111" =>  -- 128
					  if DATA_WIDTH >= 128 then
						  CaptureDR(bus_readdata(127 downto 0) & bus_ready & (not bus_ready));
						end if;						
					when others => null;
				end case;							  
			  if update_dr = '1' and bus_ready = '1' then
					case r.jbusc.dbits is
						when "00001" =>  -- 8	
						  UpdateData(8);
						when "00011" =>  -- 16 
						  if DATA_WIDTH >= 16 then
								UpdateData(16);
							end if;	
						when "00111" =>  -- 32
						  if DATA_WIDTH >= 32 then
								UpdateData(32);
							end if;						
						when "01111" =>  -- 64
						  if DATA_WIDTH >= 64 then
								UpdateData(64);
							end if;						
						when "11111" =>  -- 128
						  if DATA_WIDTH >= 128 then
								UpdateData(128);
							end if;						
						when others => null;
					end case;	
					if (v.jdata.read = '1' and r.jaddress.autoincrement = '1') or  (v.jdata.write = '1') then
						v.bus_strobe := '1';
					end if;
				end if;
				if r.jaddress.autoincrement = '1' and 
					  (update_dr = '1' or capture_dr = '1') and bus_ready = '1' then
					IncrementAddress;
				end if;
				crc_in_en := '1';
				crc_out_en := '1';				
      when JTAG_Status =>
			  CaptureDR(serial & interrupt);
				crc_out_en := '1';			
      when JTAG_Status_Control =>
			  CaptureDR(r.jstatc.serscan & r.jstatc.intscan & r.jstatc.serselect);
			  if update_dr = '1' and bus_ready = '1' then
					d7 := r.DR_Shift(r.DR_Shift'high downto r.DR_Shift'high-6);
          v.jstatc.serscan := d7(6 downto 5);
					v.jstatc.intscan := d7(4 downto 3);
					v.jstatc.serselect := d7(2 downto 0);
				end if;	
				crc_in_en := '1';
				crc_out_en := '1';				
      when JTAG_Serial_Data =>
			  -- TODO		
			when JTAG_IN_CRC =>
			  CaptureDR(crc_in_out(CRC_LENGTH-1 downto 0));
				crc_in_sclr <= capture_dr;
			when JTAG_OUT_CRC =>
			  CaptureDR(crc_out_out(CRC_LENGTH-1 downto 0));
				crc_out_sclr <= capture_dr;
      when JTAG_BYPASS =>			
				tdo <= tdi;
			when others => 
			  tdo <= tdi;
		end case;
		crc_in_enable <= crc_in_en and shift_dr;
		crc_out_enable <= crc_out_en and shift_dr;
		next_r <= v; 
	end process next_state_proc;	
	
	process(reset, fullreset, tck)
	begin
		if reset = '1' or fullreset = '1' then
			r.bus_strobe <= '1';
			r.dtmcontrol.abussize <= CONV_STD_LOGIC_VECTOR(ADDRESS_WIDTH, 8);
			if DATA_WIDTH >= 128 then
				r.dtmcontrol.access128 <= '1';
			end if;
			if DATA_WIDTH >= 64 then
				r.dtmcontrol.access64 <= '1';
			end if;
			if DATA_WIDTH >= 32 then
				r.dtmcontrol.access32 <= '1';
			end if;
			if DATA_WIDTH >= 16 then
				r.dtmcontrol.access16 <= '1';
			end if;
			r.dtmcontrol.access8 <= '1';
			r.dtmcontrol.authenticated <= '1';
			r.dtmcontrol.authbusy <= '0';
			r.dtmcontrol.authtype <= "00";
			r.dtmcontrol.ndreset <= '0';
			if fullreset = '0' then
			  r.dtmcontrol.fullreset <= '0';
			end if;	
		  r.serinfo.serial0 <= SERIAL_NOT_SUPPORTED;
		  r.serinfo.serial1 <= SERIAL_NOT_SUPPORTED;
		  r.serinfo.serial2 <= SERIAL_NOT_SUPPORTED;
		  r.serinfo.serial3 <= SERIAL_NOT_SUPPORTED;
		  r.serinfo.serial4 <= SERIAL_NOT_SUPPORTED;
		  r.serinfo.serial5 <= SERIAL_NOT_SUPPORTED;
		  r.serinfo.serial6 <= SERIAL_NOT_SUPPORTED;
		  r.serinfo.serial7 <= SERIAL_NOT_SUPPORTED;
		  r.jbusc.error <= ERROR_NONE;
		  r.jbusc.dbits <= "00111";
			r.jbusc.abits <= "101";
			r.jbusc.size <= "010";
			r.jaddress.update <= (others => '0');
			r.jaddress.autoincrement <= '0';
			r.jdata.data <= (others => '0');
			r.jdata.write <= '0';
			r.jdata.read <= '0';
			r.jstatc.serscan <= (others => '0');
			r.jstatc.intscan <= (others => '0');
			r.jstatc.serselect <= (others => '0');
			r.jserial.data <= (others => '0');
			r.jserial.write <= '0';
		elsif rising_edge(tck) then	
			r <= next_r;
		end if;
	end process;

  dbg_reset_proc: process (reset, clk)
	begin
	  if reset = '1' then
	    reset_sync1 <= '0';
	    reset_sync2 <= '0';
		  reset_syncq <= '0';
	    ndreset_sync1 <= '0';
	    ndreset_sync2 <= '0';
		  ndreset_syncq <= '0';			
	  elsif rising_edge(clk) then
		  reset_sync1 <= r.dtmcontrol.fullreset;
		  reset_sync2 <= reset_sync1;
		  reset_syncq <= reset_sync2;  -- used to detect toggles
		  ndreset_sync1 <= r.dtmcontrol.ndreset;
		  ndreset_sync2 <= ndreset_sync1;
		  ndreset_syncq <= ndreset_sync2;  -- used to detect toggles			
	  end if;
	end	process dbg_reset_proc;
	
	fullreset <= reset_sync2;
	ndreset <= ndreset_sync2;
	
	dbg_reset <= fullreset or ndreset;
	dbg_ndreset <= ndreset;
	
	bus_tck <= tck;	
	bus_strobe <= r.bus_strobe;
	bus_address <= r.jaddress.update;
	bus_size <= r.jbusc.size;
	bus_writedata <= r.jdata.data;
	bus_write <= r.jdata.write;
end behaviour;
