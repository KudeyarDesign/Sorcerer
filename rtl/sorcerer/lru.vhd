-- Project Sorcerer - RISC-V core
--
-- LRU calculation module for large LRU size (> 4)
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

entity lru is
	generic
	(
		SIZE: natural range 4 to 64 := 4
	);
  port
  (
	  clk : in std_logic;
		reset : in std_logic;
		touch: in std_logic;
		touch_pos: in std_logic_vector(ceil_log2(SIZE)-1 downto 0);
		last_pos: out std_logic_vector(ceil_log2(SIZE)-1 downto 0)
	);	
end lru;

architecture behaviour of lru is
  constant LRU_BITS: natural := ceil_log2(SIZE);
	type pos_array is array (0 to SIZE-1) of std_logic_vector(LRU_BITS-1 downto 0);
	
	type lru_regs_type is record
		last_pos: std_logic_vector(LRU_BITS-1 downto 0);
		rows: pos_array;
		up: std_logic_vector(SIZE-1 downto 0);
	end record;	

	signal r, next_r: lru_regs_type;
begin
	next_state_proc: process(r, touch, touch_pos)
	  variable v: lru_regs_type;
		variable active: std_logic_vector(SIZE-1 downto 0); 
	begin
	  v := r;	
		active := r.up;
		if touch = '1' then
			for i in 0 to SIZE-1 loop
				if touch_pos = r.rows(i) then
					active(i) := '1';
				else
					active(i) := '0';
				end if;	
			end loop;	
		end if;	
		v.up := active(SIZE-2 downto 0) & '0';
		for i in SIZE-2 downto 0 loop
			if active(i) = '1' then
				v.rows(i) := r.rows(i + 1);
				v.rows(i + 1) := r.rows(i);
			end if;	
		end loop;
		v.last_pos := v.rows(0);
		next_r <= v;
		last_pos <= r.last_pos; 
	end process next_state_proc;	
	
	process(reset, clk)
	begin
		if reset = '1' then
			for i in 0 to SIZE-1 loop
				r.rows(i) <= CONV_STD_LOGIC_VECTOR(i, LRU_BITS);
				r.last_pos <= (others => '0');
				r.up <= (others => '0');
			end loop;	
		elsif rising_edge(clk) then	
			r <= next_r;
		end if;
	end process;
end behaviour;
