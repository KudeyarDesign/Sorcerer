-- Project Sorcerer - RISC-V core
--
-- Arithmetic & Logical Unit for atomic operations (AMO, "A" extension)
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
use work.config.all;
use work.pipeline_iface.all;

entity amoalu is
	port
	(
	  din: in amoalu_in_type;
		result: out std_logic_vector(WORD_SIZE-1 downto 0)
	);	
end amoalu;

architecture behaviour of amoalu is
begin
  amoalu_proc: process(din) 
	  variable v: std_logic_vector(WORD_SIZE-1 downto 0);
		variable sub: std_logic_vector(WORD_SIZE downto 0);
		variable less: std_logic;
	begin										
		v := (others => '0');
		sub := ('0' & din.lhs) - ('0' & din.rhs);
		less := (not din.lhs(WORD_SIZE-1) and not din.rhs(WORD_SIZE-1) and sub(WORD_SIZE-1)) or
		        (din.lhs(WORD_SIZE-1) and (not din.rhs(WORD_SIZE-1) or sub(WORD_SIZE-1)));
		case din.oper is
	    when amoalu_nop =>  
			  v := din.lhs;
	    when amoalu_add =>	
			  v := din.lhs + din.rhs;
	    when amoalu_and => 
			  v := din.lhs and din.rhs;
	    when amoalu_or =>	
			  v := din.lhs or din.rhs;
	    when amoalu_xor =>
			  v := din.lhs xor din.rhs;
	    when amoalu_swap =>
				v := din.rhs;
	    when amoalu_min =>
			  if less = '1' then
					v := din.lhs;
				else	
					v := din.rhs;
				end if;			  
	    when amoalu_max =>
			  if less = '1' then
					v := din.rhs;
				else	
					v := din.lhs;
				end if;			
	    when amoalu_minu =>
			  if sub(WORD_SIZE) = '1' then
					v := din.lhs;
				else	
					v := din.rhs;
				end if;
	    when amoalu_maxu =>
			  if sub(WORD_SIZE) = '1' then
					v := din.rhs;
				else	
					v := din.lhs;
				end if;			
			when others => null;
		end case;	
		result <= v;
	end process amoalu_proc;
end behaviour;
