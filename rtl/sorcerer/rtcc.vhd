-- Project Sorcerer - RISC-V core
--
-- System Timer (RTCC) with Avalon MM Bus interface
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

entity rtcc is
	generic
	(
		CLK_FREQ: natural := 1   -- input clock frequency in MHz, default parameter for prescaler 
	);
	port
	(
	  clk: in std_logic;
		reset: in std_logic;
		
		intr: out std_logic;
		
		-- Avalon Bus Slave
		avs_s1_address: in std_logic_vector(2 downto 0);
		avs_s1_writedata: in std_logic_vector(31 downto 0); 
		avs_s1_chipselect: in std_logic;
		avs_s1_read: in std_logic;
		avs_s1_write: in std_logic;
		avs_s1_readdata: out std_logic_vector(31 downto 0)		
	);	
end rtcc;

architecture behaviour of rtcc is	
  -- RTCC register addresses (offsets in bytes)
  constant MTIME_ADDRESS: natural := 0;			 -- Machine timer register (low 32 bits)
	constant MTIMEH_ADDRESS: natural := 4;		 -- Machine timer register (high 32 bits)
  constant MTIMECMP_ADDRESS: natural := 8;	 -- Machine timer compare register (low 32 bits)
	constant MTIMECMPH_ADDRESS: natural := 12; -- Machine timer compare register (high 32 bits)
	constant PRESCALER_ADDRESS: natural := 16; -- Prescaler register

	constant MTIME_SADDR: std_logic_vector(avs_s1_address'range) := 
	  CONV_STD_LOGIC_VECTOR(MTIME_ADDRESS/4, avs_s1_address'length); 
	constant MTIMEH_SADDR: std_logic_vector(avs_s1_address'range) := 
	  CONV_STD_LOGIC_VECTOR(MTIMEH_ADDRESS/4, avs_s1_address'length);
	constant MTIMECMP_SADDR: std_logic_vector(avs_s1_address'range) := 
	  CONV_STD_LOGIC_VECTOR(MTIMECMP_ADDRESS/4, avs_s1_address'length);	
	constant MTIMECMPH_SADDR: std_logic_vector(avs_s1_address'range) := 
	  CONV_STD_LOGIC_VECTOR(MTIMECMPH_ADDRESS/4, avs_s1_address'length);	
	constant PRESCALER_SADDR: std_logic_vector(avs_s1_address'range) := 
	  CONV_STD_LOGIC_VECTOR(PRESCALER_ADDRESS/4, avs_s1_address'length);	
	
  type reg_type is record
	  presc_cnt: std_logic_vector(7 downto 0);
		presc_value: std_logic_vector(7 downto 0);
		mtime: std_logic_vector(63 downto 0);
		mtimecmp: std_logic_vector(63 downto 0);
		irq: std_logic;
	end record;	
	
	signal r, next_r: reg_type;
begin
	next_state_proc: process(r, 
	    avs_s1_address, avs_s1_chipselect, avs_s1_read, avs_s1_write, avs_s1_writedata)
	  variable v: reg_type;
		
	begin
		v := r;	
		
		if r.presc_cnt = 0 then
			v.mtime := r.mtime + 1;
			v.presc_cnt := r.presc_value;
		else	
			v.presc_cnt := r.presc_cnt - 1;
		end if;	
		
		if r.mtime > r.mtimecmp then
			v.irq := '1';
		else	
			v.irq := '0';
		end if;	
		
		-- Avalon Bus operations
    avs_s1_readdata <= (others => '0');		
		if avs_s1_chipselect = '1' then
			if avs_s1_write = '1' then
				case avs_s1_address is
					when MTIME_SADDR =>  v.mtime(31 downto 0) := avs_s1_writedata;
					when MTIMEH_SADDR =>  v.mtime(63 downto 32) := avs_s1_writedata;	
					when MTIMECMP_SADDR =>  v.mtimecmp(31 downto 0) := avs_s1_writedata;
					when MTIMECMPH_SADDR =>  v.mtimecmp(63 downto 32) := avs_s1_writedata;
					when PRESCALER_SADDR => v.presc_value := avs_s1_writedata(v.presc_value'range);
					when others => null;
				end case;	
			end if;	
			if avs_s1_read = '1' then
				case avs_s1_address is
					when MTIME_SADDR =>  avs_s1_readdata <= r.mtime(31 downto 0);
					when MTIMEH_SADDR =>  avs_s1_readdata <= r.mtime(63 downto 32);
					when MTIMECMP_SADDR =>  avs_s1_readdata <= r.mtimecmp(31 downto 0);
					when MTIMECMPH_SADDR =>  avs_s1_readdata <= r.mtimecmp(63 downto 32);
					when PRESCALER_SADDR =>  avs_s1_readdata(v.presc_value'range) <= r.presc_value;
					when others => null;
				end case;
			end if;	
		end if;		
		
		next_r <= v;
	end process next_state_proc;	

	process(reset, clk)
	begin
		if reset = '1' then
			r.presc_cnt <= CONV_STD_LOGIC_VECTOR(CLK_FREQ - 1, r.presc_value'length);
			r.presc_value <= CONV_STD_LOGIC_VECTOR(CLK_FREQ - 1, r.presc_value'length);
			r.mtime <= (others => '0');
			r.mtimecmp <= (others => '1');
			r.irq <= '0';
		elsif rising_edge(clk) then	
			r <= next_r;
		end if;	
	end process;	

	intr <= r.irq;
end behaviour;
