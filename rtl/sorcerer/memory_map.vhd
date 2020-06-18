-- Project Sorcerer - RISC-V core
--
-- Memory Map functions
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

package memory_map is
	function isCacheable(vaddress: std_logic_vector(VADDRESS_WIDTH-1 downto 0)) return std_logic;
	function mapVAddress(vaddress: std_logic_vector(VADDRESS_WIDTH-1 downto 0)) return std_logic_vector;
end package memory_map;

package body memory_map is
	function isCacheable(vaddress: std_logic_vector(VADDRESS_WIDTH-1 downto 0)) return std_logic is
	begin									
		case vaddress(VADDRESS_WIDTH-1 downto VADDRESS_WIDTH-4) is
			when "0000" =>
			  if vaddress(VADDRESS_WIDTH-4 downto 12) = 0 then
				  return '0';  -- Debug module & Debug ROM & Debug RAM
				else
					return '1';
				end if;	
			when "0001" | "0010" | "0011" =>
			  return '0';
			when "0100" =>  
			  if vaddress(VADDRESS_WIDTH-5 downto VADDRESS_WIDTH-12) = x"88" then
			    return '1';
			  else
	          return '0'; 
		     end if;
			when "1111" =>
	        return '0';		
			when others =>  return '1';
		end case;	
	end function isCacheable;
	
	function mapVAddress(vaddress: std_logic_vector(VADDRESS_WIDTH-1 downto 0))  
	    return std_logic_vector is
		variable paddress: std_logic_vector(PADDRESS_WIDTH-1 downto 0);
	begin
		paddress := vaddress;
		return paddress;
	end function mapVAddress;
end memory_map;
