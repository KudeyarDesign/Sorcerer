-- Project Sorcerer - RISC-V core
--
-- Push Button Debouncer
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

entity debouncer is
	generic
	(
	  FREQ: natural := 50000000;      -- in Hz
		DEBOUNCE_TIME: natural := 1000; -- in us
		DEFAULT_HIGH: natural range 0 to 1 := 1;
          INVERT: natural range 0 to 1 := 0
	);	
	port
	(
		reset: in std_logic;
		clk:   in std_logic;
		
		inp: in std_logic;
		outp: out std_logic
	);
end debouncer;

architecture behaviour of debouncer is
  constant CNT_MAX: natural := DEBOUNCE_TIME * (FREQ /1000) / 1000;
	constant CNT_BITS: natural := log2(CNT_MAX) + 1;
	
	type state_type is (st_stable, st_wait);
	type reg_type is record
		state: state_type;
		count: std_logic_vector(CNT_BITS-1 downto 0);
		outp: std_logic;
	end record;
	
	signal r, next_r: reg_type;
begin
	next_state_proc: process(r, inp)
	  variable v: reg_type;
	begin	
		v := r;
		case r.state is
			when st_stable =>
			  if inp /= r.outp then
					v.count := CONV_STD_LOGIC_VECTOR(CNT_MAX-1, CNT_BITS);
					v.state := st_wait;
				end if;
			when st_wait =>
			  if inp /= r.outp then	
				  if r.count = 0 then
						v.outp := inp;
						v.state := st_stable;
					end if;	
					v.count := r.count - 1;
				else	
					v.state := st_stable;
				end if;
		end case;
		next_r <= v;
	end process next_state_proc;	
	
  process(reset, clk)
	begin
		if reset = '1' then	
			r.state <= st_stable;
			r.count <= (others => '0');
			if DEFAULT_HIGH = 1 then
			  r.outp <= '1';
			else
				r.outp <= '0';
			end if;	
		elsif rising_edge(clk) then	
			r <= next_r;			
		end if;	
	end process;
	
	outp <= r.outp when INVERT = 0 else not r.outp;		
end behaviour;
