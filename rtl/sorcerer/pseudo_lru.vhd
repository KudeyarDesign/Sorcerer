-- Project Sorcerer - RISC-V core
--
-- Pseudo-LRU calculation
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

entity pseudo_lru is
	generic
	(
		SIZE: natural range 4 to 256 := 16
	);
  port
  (
	  clk : in std_logic;
		reset : in std_logic;
		touch: in std_logic;
		touch_pos: in std_logic_vector(log2(SIZE)-1 downto 0);
		last_pos: out std_logic_vector(log2(SIZE)-1 downto 0)
	);	
end pseudo_lru;

architecture behaviour of pseudo_lru is
  constant LRU_BITS: natural := log2(SIZE);

  signal r, next_r: std_logic_vector(SIZE-1 downto 0);
begin
	next_state_proc: process(r, touch, touch_pos)
	  variable v: std_logic_vector(SIZE-1 downto 0);
		variable idx: std_logic_vector(LRU_BITS-1 downto 0);
		variable tbit: std_logic;
		variable pos: std_logic_vector(LRU_BITS-1 downto 0);
	begin
	  v := r;
		idx := CONV_STD_LOGIC_VECTOR(1, LRU_BITS);
		for i in LRU_BITS-1 downto 0 loop
			tbit := touch_pos(i);
			if tbit = '1' then
				v(CONV_INTEGER(idx)) := '0';
			else	
				v(CONV_INTEGER(idx)) := '1';
			end if;	
			idx := idx(idx'high-1 downto 0) & tbit;
		end loop;		
		if touch = '1' then
			next_r <= v;
		else
			next_r <= r;
		end if;	
		
		pos := CONV_STD_LOGIC_VECTOR(1, LRU_BITS);
		for i in 0 to LRU_BITS-1 loop
			pos := pos(pos'high-1 downto 0) & r(CONV_INTEGER(pos));	
		end loop;	
		last_pos <= pos;
	end process next_state_proc;	
	
	process(reset, clk)
	begin
		if reset = '1' then
			r <= (others => '0');
		elsif rising_edge(clk) then	
			r <= next_r;
		end if;
	end process;
end behaviour;
