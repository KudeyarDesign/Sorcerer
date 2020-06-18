-- Project Sorcerer - RISC-V core
--
-- Utility functions for cache memory LRU operations
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

package cache_utils is	
  function lru_width(associativity: natural) return natural;
	procedure lru_next(lru: std_logic_vector; hit: std_logic_vector; 
	  associativity: natural; lru_out: out std_logic_vector);
	function lru_victim(lru: std_logic_vector; associativity: natural) return natural;
end package cache_utils;

package body cache_utils is
	function lru_width(associativity: natural) return natural is
	begin
		case associativity is
			when 1 | 2 =>   return 1;
			when 3 =>       return 3;
			when 4 =>       return 5;
			when others =>  return associativity - 1;
		end case;	
	end function lru_width;	

	function lru_next3(lru: std_logic_vector(2 downto 0); hit: std_logic_vector(2 downto 0)) 
	  return std_logic_vector is
	begin
		case hit is
		  when "XX1" => 
		    case lru is
		      when "010" =>  return "000";
		      when "011" =>  return "001";
		      when "100" =>  return "000";
		      when "101" =>  return "001";
		      when others => return lru;
		    end case;
		  when "X1X" => 
		    case lru is
		      when "000" =>  return "010";
		      when "001" =>  return "011";
		      when "100" =>  return "011";
		      when "101" =>  return "010";
		      when others => return lru;
		    end case;
		  when "1XX" => 
		    case lru is
		      when "000" =>  return "100";
		      when "001" =>  return "101";
		      when "010" =>  return "101";
		      when "011" =>  return "100";
		      when others => return lru;
		    end case;
			when others =>  return lru;
		end case;		
	end function lru_next3;	
	
	function lru_next4(lru: std_logic_vector(4 downto 0); hit: std_logic_vector(3 downto 0)) 
	  return std_logic_vector is
	begin
		case hit is
		  when "XXX1" => 
		    case lru is
		      when "00110" =>  return "00000";
		      when "00111" =>  return "00001";
		      when "01000" =>  return "00010";
		      when "01001" =>  return "00011";
		      when "01010" =>  return "00100";
		      when "01011" =>  return "00101";
		      when "01100" =>  return "00000";
		      when "01101" =>  return "00001";
		      when "01110" =>  return "00010";
		      when "01111" =>  return "00011";
		      when "10000" =>  return "00100";
		      when "10001" =>  return "00101";
		      when "10010" =>  return "00000";
		      when "10011" =>  return "00001";
		      when "10100" =>  return "00010";
		      when "10101" =>  return "00011";
		      when "10110" =>  return "00100";
		      when "10111" =>  return "00101";
		      when others =>   return lru;
		    end case;
		  when "XX1X" => 
		    case lru is
		      when "00000" =>  return "00110";
		      when "00001" =>  return "00111";
		      when "00010" =>  return "01000";
		      when "00011" =>  return "01001";
		      when "00100" =>  return "01010";
		      when "00101" =>  return "01011";
		      when "01100" =>  return "01000";
		      when "01101" =>  return "01001";
		      when "01110" =>  return "00110";
		      when "01111" =>  return "00111";
		      when "10000" =>  return "01011";
		      when "10001" =>  return "01010";
		      when "10010" =>  return "01010";
		      when "10011" =>  return "01011";
		      when "10100" =>  return "01001";
		      when "10101" =>  return "01000";
		      when "10110" =>  return "00110";
		      when "10111" =>  return "00111";
		      when others =>   return lru;
		    end case;
		  when "X1XX" => 
		    case lru is
		      when "00000" =>  return "01100";
		      when "00001" =>  return "01101";
		      when "00010" =>  return "01110";
		      when "00011" =>  return "01111";
		      when "00100" =>  return "10000";
		      when "00101" =>  return "10001";
		      when "00110" =>  return "01110";
		      when "00111" =>  return "01111";
		      when "01000" =>  return "01100";
		      when "01001" =>  return "01101";
		      when "01010" =>  return "10001";
		      when "01011" =>  return "10000";
		      when "10010" =>  return "01101";
		      when "10011" =>  return "01100";
		      when "10100" =>  return "10001";
		      when "10101" =>  return "10000";
		      when "10110" =>  return "01111";
		      when "10111" =>  return "01110";
		      when others =>   return lru;
		    end case;
		  when "1XXX" => 
		    case lru is
		      when "00000" =>  return "10010";
		      when "00001" =>  return "10011";
		      when "00010" =>  return "10100";
		      when "00011" =>  return "10101";
		      when "00100" =>  return "10110";
		      when "00101" =>  return "10111";
		      when "00110" =>  return "10110";
		      when "00111" =>  return "10111";
		      when "01000" =>  return "10101";
		      when "01001" =>  return "10100";
		      when "01010" =>  return "10010";
		      when "01011" =>  return "10011";
		      when "01100" =>  return "10011";
		      when "01101" =>  return "10010";
		      when "01110" =>  return "10111";
		      when "01111" =>  return "10110";
		      when "10000" =>  return "10101";
		      when "10001" =>  return "10100";
		      when others =>   return lru;
		    end case;
			when others =>  return lru;	
    end case;		
	end function lru_next4;	
	
	procedure lru_next(lru: in std_logic_vector; hit: in std_logic_vector; 
	  associativity: natural; lru_out: out std_logic_vector) is
	begin
		case associativity is
			when 1 =>
				null;  -- nothing to do			
			when 2 =>
			  lru_out(0) := hit(1);	
			when 3 =>
			  lru_out := lru_next3(lru(2 downto 0), hit(2 downto 0));			
			when 4 =>
			  lru_out := lru_next4(lru(4 downto 0), hit(3 downto 0));
			when others => 
			  null;
			   -- TODO
		end case;	
	end procedure lru_next;	
	
	function lru_victim3(lru: std_logic_vector(2 downto 0))  return natural is
	begin
		case lru is
      when "011" | "100" => return 0;
      when "001" | "101" => return 1;
      when "000" | "010" => return 2;			
			when others => return 0;
		end case;	
	end function lru_victim3;	
	
	function lru_victim4(lru: std_logic_vector(4 downto 0))  return natural is
	begin
		case lru is
			when "01001" | "01010" | "01101" | "10001" | "10010" | "10100" => return 0;
			when "00011" | "00100" | "01111" | "10000" | "10101" | "10110" => return 1;
			when "00001" | "00101" | "00111" | "01011" | "10011" | "10111" => return 2;
			when "00000" | "00010" | "00110" | "01000" | "01100" | "01110" => return 3;			
			when others => return 0;
		end case;	
	end function lru_victim4;	
	
	function lru_victim(lru: std_logic_vector; associativity: natural) return natural is
	begin
		case associativity is
			when 1 =>
				return 0; 
			when 2 =>
			  return CONV_INTEGER(not lru(0 downto 0));	
			when 3 =>
			  return lru_victim3(lru(2 downto 0));			
			when 4 =>
			  return lru_victim4(lru(4 downto 0));
			when others => 
			  return 0;
			   -- TODO
		end case;		
	end function lru_victim;	
end package body cache_utils;
