-- Project Sorcerer - RISC-V core
--
-- Compressed instructions ("C") functions
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
use work.pipeline_iface.all;

package rv32c_utils is																													
	-- Convert from 3-bit compressed register number to 5-bit full index
	function get_rvc_i(reg: in std_logic_vector(2 downto 0)) return std_logic_vector;
	
	-- Get rs1 and rs2 5-bit full indexes from 16-bit compressed instruction code
	procedure decode_c_regs(insn: in std_logic_vector(15 downto 0); 
	  rs1: out std_logic_vector(4 downto 0); rs2: out std_logic_vector(4 downto 0));
end package rv32c_utils;

package body rv32c_utils is
  function get_rvc_i(reg: in std_logic_vector(2 downto 0)) return std_logic_vector is
	begin
		return "01" & reg;				
	end function get_rvc_i;	
	
	procedure decode_c_regs(insn: in std_logic_vector(15 downto 0); 
	    rs1: out std_logic_vector(4 downto 0); rs2: out std_logic_vector(4 downto 0)) is
		variable v1: std_logic_vector(4 downto 0);
		variable v2: std_logic_vector(4 downto 0);
		variable opcode: std_logic_vector(1 downto 0);
	begin
		v1 := insn(11 downto 7);
		v2 := insn(6 downto 2);
		opcode := insn(1 downto 0);
		
		case insn(15 downto 13) is
			when "000" =>
			  if opcode = "00" then  -- C.ADDI4SPN
					v1 := SP_REG;	
				end if;

			when "001" =>	
			  null;
			
			when "010" =>
			  case opcode is
					when "00" =>  -- C.LW
					  v1 := get_rvc_i(insn(9 downto 7));
					when others => null;
				end case;

			when "011" =>
			  null;
				
			when "100" =>
			  case opcode is
					when "01" => 
					  v1 := get_rvc_i(insn(9 downto 7));
						v2 := get_rvc_i(insn(4 downto 2));
					when others => null;
				end case;
				
			when "101" =>
			  null;
				
			when "110" =>	
			  case opcode is
					when "00" =>  -- C.SW
					  v1 := get_rvc_i(insn(9 downto 7));
						v2 := get_rvc_i(insn(4 downto 2));
					when "01" =>  -- C.BEQZ
					  v1 := get_rvc_i(insn(9 downto 7));
					when others => null;
				end case;				
				
			when "111" =>	
			  case opcode is
					when "01" =>  -- C.BNEZ
					  v1 := get_rvc_i(insn(9 downto 7));
					when others => null;
				end case;				
				
			when others => null;
		end case;	
		
		rs1 := v1;
		rs2 := v2;
	end procedure decode_c_regs;	
	
end package body rv32c_utils;
