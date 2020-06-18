-- Project Sorcerer - RISC-V core
--
-- Debug Module to Avalon MM Bus bridge
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
use work.utils.all;

entity dbg_avalon_bridge is
	generic
	(
	  DATA_WIDTH: natural range 32 to 128 := 32;
		ADDRESS_WIDTH: natural range 32 to 128 := 32
	);
  port
  (
	  clk : in std_logic;   -- Avalon bus clock
		reset : in std_logic;
		
		tck: in std_logic;		-- JTAG clock
		
		bus_address: in std_logic_vector(ADDRESS_WIDTH-1 downto 0);
		bus_size: in std_logic_vector(2 downto 0);
		bus_strobe: in std_logic;
		bus_writedata: in std_logic_vector(DATA_WIDTH-1 downto 0);
		bus_write: in std_logic;
		bus_readdata: out std_logic_vector(DATA_WIDTH-1 downto 0);
		bus_ready: out std_logic;
		
		-- Avalon Master
		avm_dbgm_address: out std_logic_vector(ADDRESS_WIDTH-1 downto 0);
		avm_dbgm_read: out std_logic;
		avm_dbgm_readdata: in std_logic_vector(DATA_WIDTH-1 downto 0);
		avm_dbgm_readdatavalid: in std_logic;
		avm_dbgm_write: out std_logic;
		avm_dbgm_writedata: out std_logic_vector(DATA_WIDTH-1 downto 0);
		avm_dbgm_byteenable: out std_logic_vector(DATA_WIDTH/8-1 downto 0);
		avm_dbgm_waitrequest: in std_logic		
	);	
end dbg_avalon_bridge;

architecture behaviour of dbg_avalon_bridge is
  constant LOW_ADDRESS_BIT: natural := log2(DATA_WIDTH/8);

  type state_type is (st_idle, st_read, st_wait_read, st_write);
	type bridge_regs_type is record
		state: state_type;
		readdata: std_logic_vector(DATA_WIDTH-1 downto 0);
	end record;	
	
	signal r, next_r: bridge_regs_type;
	
  signal start_sync: std_logic;  -- This is 'active-toggle' 
  signal rdy_sync: std_logic;    -- Also active-toggle	
	
  -- Sync registers.  "tff" indicates tck domain, "avff" indicates Avalon clk domain
  signal rdy_sync_tff1: std_logic;
  signal rdy_sync_tff2: std_logic;
  signal rdy_sync_tff2q: std_logic;     -- used to detect toggles
  signal start_sync_avff1: std_logic;
  signal start_sync_avff2: std_logic;
  signal start_sync_avff2q: std_logic;  -- used to detect toggles	
	
  signal start_toggle: std_logic;  -- Avalon domain, indicates a toggle on the start strobe
	signal rdy: std_logic;
	signal rdy_sync_en: std_logic;
	signal rd_en: std_logic;	
begin	
	------- JTAG TCK Clock domain ------		
	

  -- Create toggle-active strobe signal for clock sync.  This will start a transaction
  -- on the Avalon once the toggle propagates to the FSM in the Avalon domain.
  start_sync_proc: process (reset, tck)
  begin
	  if reset = '1' then
			start_sync <= '0';
	  elsif rising_edge(tck) then
			if bus_strobe = '1' and rdy = '1' then
			  start_sync <= not start_sync;
			end if;
	  end if;		
  end	process start_sync_proc;	
	
   -- Create rdy output. Set on reset, clear on strobe (if set), set on input toggle
  rdy_proc: process (reset, tck)
  begin
	  if reset = '1' then
      rdy_sync_tff1 <= '1';
      rdy_sync_tff2 <= '1';
      rdy_sync_tff2q <= '1';
      rdy <= '1'; 
	  elsif rising_edge(tck) then  
	    rdy_sync_tff1 <= rdy_sync;        -- Synchronize the ready signal across clock domains
	    rdy_sync_tff2 <= rdy_sync_tff1;
	    rdy_sync_tff2q <= rdy_sync_tff2;  -- used to detect toggles

	    if bus_strobe = '1' and rdy = '1' then
			  rdy <= '0';
	    elsif (rdy_sync_tff2 xor rdy_sync_tff2q) = '1' then
			  rdy <= '1';
      end if;					 
	  end if;	 
	end process rdy_proc;	
	
	bus_ready <= rdy;
	
	------- Avalon MM Clock domain ------	
	
  -- synchronize the start strobe
  start_proc: process (reset, clk)
	begin
	  if reset = '1' then
	    start_sync_avff1 <= '0';
	    start_sync_avff2 <= '0';
		  start_sync_avff2q <= '0';      
	  elsif rising_edge(clk) then
		  start_sync_avff1 <= start_sync;
		  start_sync_avff2 <= start_sync_avff1;
		  start_sync_avff2q <= start_sync_avff2;  -- used to detect toggles
	  end if;
	end	process start_proc;
	
	start_toggle <= (start_sync_avff2 xor start_sync_avff2q);	

  -- Create a toggle-active ready signal to send to the TCK domain
  rdy_sync_proc: process (reset, clk)
  begin
	  if reset = '1' then
			rdy_sync <= '0';
	  elsif rising_edge(clk) then
			if rdy_sync_en = '1' then
				rdy_sync <= not rdy_sync;
			end if;
		end if;	
  end process rdy_sync_proc;	
	
	next_state_proc: process(r,
	    start_toggle, bus_write, 
	    avm_dbgm_waitrequest, avm_dbgm_readdatavalid, avm_dbgm_readdata)
	  variable v: bridge_regs_type;
	begin
		v := r;	
		rdy_sync_en <= '0';
		rd_en <= '0';
		avm_dbgm_read <= '0';
		avm_dbgm_write <= '0';
		case r.state is
			when st_idle =>
			  if start_toggle = '1' then
					if bus_write = '1' then
						v.state := st_write;
					else
						v.state := st_read;
					end if;	
				end if;	
			when st_read =>
				avm_dbgm_read <= '1';
			  if avm_dbgm_readdatavalid = '1' then
					rd_en <= '1';
					rdy_sync_en <= '1';
					v.readdata := avm_dbgm_readdata;
					v.state := st_idle;
				elsif avm_dbgm_waitrequest = '0' then
					v.state := st_wait_read;	
				end if;				
			when st_wait_read =>
			  if avm_dbgm_readdatavalid = '1' then
					rd_en <= '1';
					rdy_sync_en <= '1';
					v.readdata := avm_dbgm_readdata;
					v.state := st_idle;
				end if;			
			when st_write =>
			  avm_dbgm_write <= '1';
			  if avm_dbgm_waitrequest = '0' then
				  rdy_sync_en <= '1';
					v.state := st_idle;
				end if;			
		end case;
		next_r <= v;
	end process next_state_proc;	
	
	align_proc: process(r, bus_address, bus_writedata, bus_size)
	  variable rddata: std_logic_vector(DATA_WIDTH-1 downto 0);
		variable wrdata: std_logic_vector(DATA_WIDTH-1 downto 0);
		variable byteen: std_logic_vector(DATA_WIDTH/8-1 downto 0);
		
		procedure ReadAlign(rsize: integer) is
		begin
			if DATA_WIDTH = rsize then
				rddata(rsize-1 downto 0) := r.readdata(rsize-1 downto 0);
			elsif DATA_WIDTH > rsize then	
				for i in 0 to DATA_WIDTH/rsize-1 loop
					if i = CONV_INTEGER(bus_address(LOW_ADDRESS_BIT-1 downto log2(rsize/8))) then
						rddata(rsize-1 downto 0) := r.readdata((i+1)*rsize-1 downto i*rsize);
					end if;	
				end loop;	
			end if;	
		end procedure ReadAlign;
		
		procedure WriteAlign(rsize: integer) is
		begin
			if DATA_WIDTH >= rsize then	
				for i in 0 to DATA_WIDTH/rsize-1 loop
					wrdata((i+1)*rsize-1 downto i*rsize) := bus_writedata(rsize-1 downto 0);
				end loop;	
			end if;	
		end procedure WriteAlign;
		
		procedure ByteEnable(rsize: integer) is
		begin
			if DATA_WIDTH >= rsize then	
				for i in 0 to DATA_WIDTH/rsize-1 loop
					if i = CONV_INTEGER(bus_address(LOW_ADDRESS_BIT-1 downto log2(rsize/8))) then
						for j in 0 to rsize/8-1 loop
							byteen(i*rsize/8 + j) := '1';
						end loop;	
					end if;					
				end loop;	
			end if;			
		end procedure ByteEnable;	
		
	begin
		rddata := (others => '0');
		wrdata := (others => '0');
		byteen := (others => '0');
		case bus_size is		
			when "000" =>  -- size = 8
			  ReadAlign(8);
				WriteAlign(8);
				ByteEnable(8);
			when "001" =>  -- size = 16
			  ReadAlign(16);
				WriteAlign(16);
				ByteEnable(16);
			when "010" =>  -- size = 32
			  ReadAlign(32);
				WriteAlign(32);
				ByteEnable(32);
			when "011" =>  -- size = 64
			  ReadAlign(64);
				WriteAlign(64);	
				ByteEnable(64);
			when "100" =>  -- size = 128
			  ReadAlign(128);
				WriteAlign(128);
				ByteEnable(128);
			when others => null;
		end case;	
		bus_readdata <= rddata;
		avm_dbgm_writedata <= wrdata;
		avm_dbgm_byteenable <= byteen;
	end process align_proc;	
	
	process(reset, clk)
	begin
		if reset = '1' then
			r.state <= st_idle;
		elsif rising_edge(clk) then	
			r <= next_r;
		end if;
	end process;
	
  avm_dbgm_address(ADDRESS_WIDTH-1 downto LOW_ADDRESS_BIT) <= bus_address(ADDRESS_WIDTH-1 downto LOW_ADDRESS_BIT);
	lb11: if LOW_ADDRESS_BIT > 0 generate
	  avm_dbgm_address(LOW_ADDRESS_BIT-1 downto 0) <= (others => '0');
	end generate;
end behaviour;
