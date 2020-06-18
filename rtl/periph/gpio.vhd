-- Project Sorcerer - RISC-V core
--
-- General Purpose Input/Output Module (GPIO) for Avalon MM Bus
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

entity gpio is
	generic
	(	
		PORT_NUM: natural range 1 to 32 := 16
	);
	port
	(
	  clk: in std_logic;
		reset: in std_logic;

		intr: out std_logic;		 -- Interrupt output 

		port_io: inout std_logic_vector(PORT_NUM-1 downto 0);  -- GPIO external pins
		
		-- Avalon Bus Slave 
		avs_s1_address: in std_logic_vector(3 downto 0);
		avs_s1_writedata: in std_logic_vector(31 downto 0); 
		avs_s1_chipselect: in std_logic;
		avs_s1_read: in std_logic;
		avs_s1_write: in std_logic;				 
    avs_s1_readdata: out std_logic_vector(31 downto 0)		
  );		
end gpio;

architecture behaviour of gpio is
  -- Advanced Timer register addresses (in bytes)
  constant GPIO_TRIS_ADDRESS: natural := 0;
  constant GPIO_TRIS_CLR_ADDRESS: natural := 4;	
	constant GPIO_TRIS_SET_ADDRESS: natural := 8;	
	constant GPIO_PORT_ADDRESS: natural := 12;
	constant GPIO_PORT_CLR_ADDRESS: natural := 16;
	constant GPIO_PORT_SET_ADDRESS: natural := 20;
	constant GPIO_PORT_TOGGLE_ADDRESS: natural := 24;	
	constant GPIO_LAT_ADDRESS: natural := 28;
	constant GPIO_IE_ADDRESS: natural := 32;
	constant GPIO_CLR_IF_ADDRESS: natural := 36;
	constant GPIO_CNCON_ADDRESS: natural := 40;

  type reg_type is record
	  tris: std_logic_vector(PORT_NUM-1 downto 0);				 -- Data Direction Register
		lat: std_logic_vector(PORT_NUM-1 downto 0);					 -- Latch Register
		cncon: std_logic_vector(PORT_NUM-1 downto 0);				 -- Interrupt-on-Change Control Register
		inp_sync1, inp_sync2: std_logic_vector(PORT_NUM-1 downto 0);  -- Input Synchronization Registers 
		port_val: std_logic_vector(PORT_NUM-1 downto 0);     -- Current Input Port Value
		ie: std_logic;                                       -- Interrupt Enable
		iff: std_logic_vector(PORT_NUM-1 downto 0);			     -- Interrupt Flags
	end record;
	
	signal r, next_r: reg_type;	
begin
	next_state_proc: process(r, port_io, 
	    avs_s1_chipselect, avs_s1_read, avs_s1_write, avs_s1_writedata)
	  variable v: reg_type;
		variable port_val: std_logic_vector(PORT_NUM-1 downto 0);
		variable slv_addr: natural range 0 to 15;
		variable readdata: std_logic_vector(31 downto 0);
		variable writedata: std_logic_vector(31 downto 0);		
	begin	
		v := r;
		port_val := r.inp_sync2;
		
		v.port_val := port_val;
		v.inp_sync2 := r.inp_sync1;
		v.inp_sync1 := port_io;
		
		if r.ie = '1' then
			v.iff := r.iff or ((port_val xor r.port_val) and r.cncon);
		end if;	
		
		-- Avalon Bus operations
    readdata := (others => '0');
		writedata := avs_s1_writedata;
		slv_addr := CONV_INTEGER(avs_s1_address);
		if avs_s1_chipselect = '1' then
			if avs_s1_write = '1' then 
				case slv_addr is
          when GPIO_TRIS_ADDRESS/4 =>
					  v.tris := writedata(PORT_NUM-1 downto 0);	
          when GPIO_TRIS_CLR_ADDRESS/4 =>
					  v.tris := r.tris and not writedata(PORT_NUM-1 downto 0);
          when GPIO_TRIS_SET_ADDRESS/4 =>
					  v.tris := r.tris or writedata(PORT_NUM-1 downto 0);					
					when GPIO_PORT_ADDRESS/4 =>
						v.lat := writedata(PORT_NUM-1 downto 0);
          when GPIO_PORT_CLR_ADDRESS/4 =>
						v.lat := r.lat and not writedata(PORT_NUM-1 downto 0);
          when GPIO_PORT_SET_ADDRESS/4 =>	
						v.lat := r.lat or writedata(PORT_NUM-1 downto 0);
          when GPIO_PORT_TOGGLE_ADDRESS/4 => 
					  v.lat := r.lat xor writedata(PORT_NUM-1 downto 0);
          when GPIO_IE_ADDRESS/4 =>
					  v.ie := writedata(0);
          when GPIO_CLR_IF_ADDRESS/4 =>
					  v.iff := r.iff and not writedata(PORT_NUM-1 downto 0);
          when GPIO_CNCON_ADDRESS/4	=>
					  v.cncon := writedata(PORT_NUM-1 downto 0);
					when others => null;
				end case;	
			end if;	
			if avs_s1_read = '1' then
				case slv_addr is
          when GPIO_TRIS_ADDRESS/4 =>
					  readdata(PORT_NUM-1 downto 0) := r.tris;
					when GPIO_PORT_ADDRESS/4 =>
					  readdata(PORT_NUM-1 downto 0) := port_val;
					when GPIO_LAT_ADDRESS/4 => 
					  readdata(PORT_NUM-1 downto 0) := r.lat;
          when GPIO_IE_ADDRESS/4 =>
					  readdata(0) := r.ie;
          when GPIO_CLR_IF_ADDRESS/4 =>
					  readdata(PORT_NUM-1 downto 0) := r.iff;
          when GPIO_CNCON_ADDRESS/4	=>	
					  readdata(PORT_NUM-1 downto 0) := r.cncon;
					when others => null;					
				end case;				
			end if;	
		end if;

		next_r <= v;
	end process next_state_proc;	
	
	process(reset, clk)
	begin
		if reset = '1' then	
			r.tris <= (others => '1');
			r.lat <= (others => '0');
			r.cncon <= (others => '0');
			r.inp_sync1 <= (others => '0');
			r.inp_sync2 <= (others => '0');
			r.ie <= '0';
			r.iff <= (others => '0');
		elsif rising_edge(clk) then
			r <= next_r;
		end if;	
	end process;	
	
	intr <= '1' when r.ie = '1' and r.iff /= 0 else '0';
	port_out: for i in 0 to PORT_NUM-1 generate
		port_io(i) <= r.lat(i) when r.tris(i) = '0' else 'Z';
	end generate;	
end behaviour;	