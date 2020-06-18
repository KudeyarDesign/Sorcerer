-- Project Sorcerer - RISC-V core
--
-- Barrel Shifter
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
use work.config.all;
use work.utils.all;

entity barrel_shifter is
	port
	(
	  a: in std_logic_vector(WORD_SIZE-1 downto 0);
		amount: in std_logic_vector(7 downto 0);
		right: in std_logic;
		sign: in std_logic;
		result: out std_logic_vector(WORD_SIZE-1 downto 0)
	);
end barrel_shifter;

architecture behaviour of barrel_shifter is
  
begin																		 
  shifter_proc: process (a, right, sign)
	  variable b: std_logic_vector(log2(WORD_SIZE)-1 downto 0);
    variable shiftL: std_logic_vector(WORD_SIZE-1 downto 0);
    variable shiftR: std_logic_vector(WORD_SIZE-1 downto 0);   
  begin
		b := amount(log2(WORD_SIZE)-1 downto 0); 
		shiftL := barrel_shift_l(a, b)(WORD_SIZE-1 downto 0);
		shiftR := barrel_shift_r(a, b, sign);

		if amount(amount'high downto log2(WORD_SIZE)) /= 0 then
			shiftL := (others => '0');
			if sign = '1' and a(a'high) = '1' then
				shiftR := (others => '1'); 
			else	
				shiftR := (others => '0');
			end if;	
		end if;	
		
		if right = '1' then
			result <= shiftR;
		else									
			result <= shiftL;
		end if;	

	end process shifter_proc;	
end behaviour;
