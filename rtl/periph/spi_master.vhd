-- Project Sorcerer - RISC-V core
--
-- SPI Master for Avalon MM Bus
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

entity spi_master is
	generic
	(
	  DLYBS: natural range 1 to 15 := 2;  -- Delay Before SCLK
		DLYBCT: natural range 0 to 15 := 2; -- Delay Between Consecutive Transfers
		DLYBCS: natural range 1 to 15 := 2  -- Delay between chip selects
	);
	port
	(
	  clk: in std_logic;
		reset: in std_logic;

		sck: out std_logic;
		mosi: out std_logic;
		miso: in std_logic;
		ss0: out std_logic;
		ss1: out std_logic;
		ss2: out std_logic;
		ss3: out std_logic;
		
		intr: out std_logic;															 -- Interrupt output
		
		-- Avalon Bus Slave
		avs_s1_address: in std_logic_vector(1 downto 0);
		avs_s1_writedata: in std_logic_vector(31 downto 0); 
		avs_s1_chipselect: in std_logic;
		avs_s1_read: in std_logic;
		avs_s1_write: in std_logic;
		avs_s1_readdata: out std_logic_vector(31 downto 0)		
	);	
end spi_master;

architecture behaviour of spi_master is
  type state_type is (st_idle, st_dlybs, st_shift, st_dlybct);
  type reg_type is record	
	  -- Control bits
		ONN: std_logic;	
		IFL: std_logic;                      -- Interrupt Flag
		MODE: std_logic_vector(1 downto 0);  -- "00" - 8-bit data width, "01" - 16-bit data width	
		CPOL: std_logic;                     -- Clock Polarity
		CPHA: std_logic;                     -- Clock Phase
		SSEN: std_logic;                     -- Slave Select enable
		SSADR: std_logic_vector(2 downto 0); -- Slave Address
		-- Status bits
		BUSY: std_logic;                     -- '1' = SPI peripheral is currently busy with some transactions 
		ROV: std_logic;                      -- Receive Overflow Flag bit, '1' - A new data is completely received and discarded. 
		                                     -- The user software has not read the previous data in the BUF register
		TBE: std_logic;                      -- Transmit Buffer TXB is empty	
		RBF: std_logic;                      -- Receive Buffer RXB is full
		-- Registers
	  TXB: std_logic_vector(15 downto 0);	 -- Transmit Data Buffer
		RXB: std_logic_vector(15 downto 0);	 -- Receive Data Buffer
		TSR: std_logic_vector(15 downto 0);  -- Transmit Shift register
		RSR: std_logic_vector(15 downto 0);  -- Receive Shift register
		BRG: std_logic_vector(8 downto 0);	 -- Baud Generator register
		psc_cnt: std_logic_vector(8 downto 0);  -- Prescaler counter
		sck_tick: std_logic; 
		sck: std_logic;
		bit_cnt: std_logic_vector(4 downto 0);
		delay_cnt: std_logic_vector(3 downto 0);
		state: state_type;
		sr_full: std_logic;
	end record;

	signal r, next_r: reg_type;
begin
	next_state_proc: process(r,
	    avs_s1_chipselect, avs_s1_read, avs_s1_write, avs_s1_address, avs_s1_writedata)
	  variable v: reg_type;
		variable vshift_en: std_logic;
		variable vsamp_en: std_logic;
		variable vss: std_logic_vector(7 downto 0);
		
		procedure start_shift is
		begin
		  v.TSR := r.TXB;
			v.TBE := '1';
			case r.MODE is
				when "00" =>  v.bit_cnt := "00111";
				when "01" =>	v.bit_cnt := "01111";
				when "10" =>	v.bit_cnt := "11111";
				when others => null;
			end case;	
		end procedure start_shift;
		
		procedure next_transfer is
		begin
			if r.TBE = '0' then
				start_shift;																
				v.state := st_shift;
			else
				v.delay_cnt := CONV_STD_LOGIC_VECTOR(DLYBCS-1, r.delay_cnt'length);
				v.state := st_idle;
			end if;				
		end procedure next_transfer;	
	begin
		v := r;
		vss := (others => '1');
		
		if r.ONN = '1' then
			vshift_en := '0';
			vsamp_en := '0';			
		  if r.psc_cnt = 0 then
				v.psc_cnt := r.BRG;
			  v.sck_tick := not r.sck_tick;
				vshift_en := not r.sck_tick;
				vsamp_en := r.sck_tick;
			else
				v.psc_cnt := r.psc_cnt - 1;
			end if;
			
			if vshift_en = '1' then
				case r.state is
					when st_idle =>
						if r.delay_cnt = 0 then
					    if r.TBE = '0' then	 
								v.delay_cnt := CONV_STD_LOGIC_VECTOR(DLYBS-1, r.delay_cnt'length);
							  v.state := st_dlybs;
					    end if;	
						else
							v.delay_cnt := r.delay_cnt - 1;
						end if;	
					when st_dlybs =>
						if r.delay_cnt = 0 then
							start_shift;
							v.state := st_shift;
						else
							v.delay_cnt := r.delay_cnt - 1;
						end if;	
					when st_shift =>
						v.TSR := r.TSR(14 downto 0) & '1';
						if r.bit_cnt = 0 then
							v.sr_full := '1';
							if DLYBCT /= 0 then	
								v.delay_cnt := CONV_STD_LOGIC_VECTOR(DLYBCT-1, r.delay_cnt'length);
							  v.state := st_dlybct;
							else	
								next_transfer;
							end if;							
						else
							v.bit_cnt := r.bit_cnt - 1;
						end if;	
					when st_dlybct =>	
						if r.delay_cnt = 0 then
							next_transfer;
					  else
							v.delay_cnt := r.delay_cnt - 1;
					  end if;	
			  end case;	 
			end if;

			if r.state = st_shift and vsamp_en = '1' then
				v.RSR := r.RSR(14 downto 0) & miso;
			end if;	
			
			if r.sr_full = '1' then
				if r.RBF = '0' then
					case r.MODE is 
						when "00" =>	v.RXB := x"00" & r.RSR(7 downto 0);
						when "01" =>  v.RXB := r.RSR;
						when others => null;
					end case;	
					v.RBF := '1';
					v.sr_full := '0';
				else
				  if r.state /= st_shift and v.state = st_shift	then
						v.ROV := '1';
						v.sr_full := '0';
					end if;	
				end if;
			end if;	
			
			if r.CPHA = '0' then
				case r.state is
					when st_idle =>	 
					  v.sck := r.CPOL;
					when st_shift =>
						if vsamp_en = '1' then
							v.sck := not r.CPOL;
						end if;
						if vshift_en = '1' then
							v.sck := r.CPOL;
						end if;					  
					when others => null;
				end case;	
			else	
				case v.state is
					when st_idle =>	 
					  v.sck := r.CPOL;
					when st_shift =>
						if vsamp_en = '1' then
							v.sck := r.CPOL;
						end if;
						if vshift_en = '1' then
							v.sck := not r.CPOL;
						end if;					  
					when others => null;
				end case;				
			end if;	
		end if;	
		if v.state /= st_idle then
			v.BUSY := '1';
		else	
			v.BUSY := '0';
		end if;	

		-- Avalon Bus operations 
		avs_s1_readdata <= (others => '0');
		if avs_s1_chipselect = '1' then
			if avs_s1_write = '1' then
				case avs_s1_address is
					when "00" =>	 -- Control register
					  v.ONN := avs_s1_writedata(0);
						v.IFL := avs_s1_writedata(1);
						v.MODE := avs_s1_writedata(3 downto 2);
						v.CPOL := avs_s1_writedata(4);
						v.CPHA := avs_s1_writedata(5);
						v.SSEN := avs_s1_writedata(6);
						v.SSADR := avs_s1_writedata(10 downto 8);
					when "01" =>  -- Status register
					  null;
					when "10" =>  -- TX Buffer
					  v.TXB := avs_s1_writedata(v.TXB'high downto 0);
						v.TBE := '0';
					when "11" =>  -- Baud Generator register
					  v.BRG := avs_s1_writedata(v.BRG'high downto 0);
						v.psc_cnt := v.BRG;
					when others => null;
				end case;	
			end if;
			if avs_s1_read = '1' then
				case avs_s1_address is
					when "00" =>	 -- Control register
					  avs_s1_readdata(0) <= r.ONN;
						avs_s1_readdata(1) <= r.IFL;
						avs_s1_readdata(3 downto 2) <= r.MODE;
						avs_s1_readdata(4) <= r.CPOL;
						avs_s1_readdata(5) <= r.CPHA;
						avs_s1_readdata(6) <= r.SSEN;
						avs_s1_readdata(10 downto 8) <= r.SSADR;
					when "01" =>	-- Status register
					  avs_s1_readdata(0) <= r.RBF;
						avs_s1_readdata(1) <= r.TBE;
						avs_s1_readdata(2) <= r.ROV;
						avs_s1_readdata(3) <= r.BUSY;
					when "10" =>	-- RX Buffer
					 avs_s1_readdata(v.RXB'high downto 0) <= r.RXB;
					 v.RBF := '0';
					when "11" =>
					  avs_s1_readdata(v.BRG'high downto 0) <= r.BRG;
					when others => null;
				end case;				
			end if;	
		end if;	
		if r.SSEN = '1' and r.state /= st_idle then
			vss(CONV_INTEGER(r.SSADR)) := '0';
		end if;	
		
		next_r <= v;
		intr <= r.IFL;
		sck <= r.sck;
		case r.MODE is
			when "00" =>		-- 8-bit data width
			  mosi <= r.TSR(7);
			when "01" =>	 -- 16-bit data width
			  mosi <= r.TSR(15);
			when others =>  mosi <= '0';
		end case;		
		ss0 <= vss(0);
		ss1 <= vss(1);
		ss2 <= vss(2);
		ss3 <= vss(3);
	end process next_state_proc;	

  process(reset, clk)
	begin
		if reset = '1' then	
			r.ONN <= '0';
			r.IFL <= '0';
			r.MODE <= "00";
			r.CPOL <= '0';
			r.CPHA <= '0';
			r.SSEN <= '0';
			r.SSADR <= (others => '0');
			r.BUSY <= '0';
			r.ROV <= '0';
			r.TBE <= '1';
			r.RBF <= '0';
			r.TXB <= (others => '0');
			r.RXB <= (others => '0');
			r.TSR <= (others => '0');
			r.RSR <= (others => '0');
			r.BRG <= (others => '0');
			r.psc_cnt <= (others => '0');
			r.sck_tick <= '0';
			r.sck <= '0';	
			r.bit_cnt <= (others => '0');
			r.delay_cnt <= (others => '0');
			r.state <= st_idle;
			r.sr_full <= '0';
		elsif rising_edge(clk) then	
			r <= next_r;			
		end if;	
	end process;
end behaviour;
