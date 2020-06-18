-- Project Sorcerer - RISC-V core
--
-- Instruction Disassembler for "C" compressed instructions
-- Useful for instruction trace during simulation
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
use work.utils.all;
use work.disassembler_utils.all;
use work.rv32c_utils.all;
use work.pipeline_iface.all;

package disassembler_rv32c is
	function disassemble_rv32c(insn: std_logic_vector(15 downto 0); 
	    pc: std_logic_vector(31 downto 0)) return string;
end package disassembler_rv32c;

package body disassembler_rv32c is
	function imm_toString(imm: std_logic_vector) return string is
	    variable imm32: std_logic_vector(31 downto 0);
	  begin
		imm32 := sign_extend32(imm);
		if imm32(imm32'high) = '1' then
			imm32(imm32'high) := '0';
			return "-" & toDecString(CONV_INTEGER(imm32(30 downto 0)));
		else																	
			return toDecString(CONV_INTEGER(imm32(30 downto 0)));
		end if;	
	end function imm_toString;	
	
	function disassemble_rv32c(insn: std_logic_vector(15 downto 0); 
	    pc: std_logic_vector(31 downto 0)) return string is
		variable opcode: std_logic_vector(1 downto 0);
		variable funct3: std_logic_vector(2 downto 0);
		variable funct4: std_logic_vector(3 downto 0);
		
		function CR(name: string) return string is
		  variable rs2: std_logic_vector(4 downto 0);
		  variable rd: std_logic_vector(4 downto 0);		
		begin
			rs2 := get_rvc_i(insn(4 downto 2));
			rd := get_rvc_i(insn(9 downto 7));
			return name & " " & get_register_name(rd) & ", " & get_register_name(rs2);			
		end function CR;
		
		function MV return string is
		  variable rs2: std_logic_vector(4 downto 0);
		  variable rd: std_logic_vector(4 downto 0);		
		begin											
			rs2 := insn(6 downto 2);
			rd := insn(11 downto 7);
			return "c.mv" & " " & get_register_name(rd) & ", " & get_register_name(rs2); 
		end function MV;	
		
		function CI(name: string; add: boolean) return string is
		  variable rd: std_logic_vector(4 downto 0);
			variable imm: std_logic_vector(5 downto 0);
		begin												
			if add then
				rd := insn(11 downto 7);
			else	
			  rd := get_rvc_i(insn(9 downto 7));
			end if;	
			imm := insn(12) & insn(6 downto 2);
			return name & " " & get_register_name(rd) & ", " & imm_toString(imm);
		end function CI;
		
		function CLI(name: string) return string is
		  variable rd: std_logic_vector(4 downto 0);
			variable imm: std_logic_vector(5 downto 0);
		begin
			rd := insn(11 downto 7);
			imm := insn(12) & insn(6 downto 2);
			return name & " " & get_register_name(rd) & ", " & imm_toString(imm);
		end function CLI;		
		
		function CLUI return string is
		  variable rd: std_logic_vector(4 downto 0);
		  variable imm: std_logic_vector(17 downto 0);
		begin																					
			rd := insn(11 downto 7);
			imm(17) := insn(12);
			imm(16 downto 12) := insn(6 downto 2);
			imm(11 downto 0) := (others => '0');
			return "c.lui" & " " & get_register_name(rd) & ", " & "0x" & toHexString(CONV_INTEGER(imm));
		end function CLUI;	
		
		function CSHI(name: string; rd: std_logic_vector(4 downto 0)) return string is
			variable imm: std_logic_vector(5 downto 0);		  
		begin								
			imm := insn(12) & insn(6 downto 2);
			return name & " " & get_register_name(rd) & ", " & toDecString(CONV_INTEGER(imm));
		end function CSHI;
		
		function CBR(name: string) return string is
		  variable rs1: std_logic_vector(4 downto 0);
		  variable branch_offset: std_logic_vector(8 downto 0);
		begin
			rs1 := get_rvc_i(insn(9 downto 7));
			branch_offset(8) := insn(12);
			branch_offset(4 downto 3) := insn(11 downto 10);
			branch_offset(5 downto 4) := insn(6 downto 5);
			branch_offset(2 downto 1) := insn(4 downto 3);
			branch_offset(5) := insn(2);
			branch_offset(0) := '0';
			return name & " " & get_register_name(rs1) & ", " & imm_toString(branch_offset);
		end function CBR;
		
		function CJAL(link: boolean) return string is
		  variable rd: std_logic_vector(4 downto 0); 
			variable branch_offset: std_logic_vector(11 downto 0);
		begin
			if link then
				rd := RA_REG;
			else	
				rd := ZERO_REG;
			end if;	
			branch_offset(11) := insn(12);
			branch_offset(4) := insn(11);
			branch_offset(9 downto 8) := insn(10 downto 9);	
			branch_offset(10) := insn(8);
			branch_offset(6) := insn(7);
			branch_offset(7) := insn(6);
			branch_offset(3 downto 1) := insn(5 downto 3);
			branch_offset(5) := insn(2);
			branch_offset(0) := '0';
			return "c.jal" & " " & get_register_name(rd) & ", " & imm_toString(branch_offset);
		end function CJAL;
		
		function CJALR(link: boolean) return string is 
		  variable rs1: std_logic_vector(4 downto 0);
			variable rd: std_logic_vector(4 downto 0);
		begin
			rs1 := insn(11 downto 7);
			if link then
				rd := RA_REG;
			else	
				rd := ZERO_REG;
			end if;			
			return "c.jalr" & " " & get_register_name(rs1) & ", " & get_register_name(rd);
		end function CJALR;
		
		function CADDI4SPN return string is
		  variable rd: std_logic_vector(4 downto 0);
			variable imm: std_logic_vector(9 downto 0);
		begin
			rd := get_rvc_i(insn(4 downto 2));
			imm(5 downto 4) := insn(12 downto 11);
			imm(9 downto 6) := insn(10 downto 7);
			imm(2) := insn(6);
			imm(3) := insn(5);
			imm(1 downto 0) := "00";
			return "c.addi4spn" & " " & get_register_name(rd) & ", " & "sp" & ", " & 
			  toDecString(CONV_INTEGER(imm));
		end function CADDI4SPN;
		
		function CADDI16SP return string is
		  variable imm: std_logic_vector(9 downto 0); 
		begin
			imm(9) := insn(12);
			imm(4) := insn(6);
			imm(6) := insn(5);
			imm(8 downto 7) := insn(4 downto 3);
			imm(5) := insn(2);
			imm(3 downto 0) := "0000";			
			return "c.addi16sp" & " " & "sp, " & imm_toString(imm);
		end function CADDI16SP;	
		
		function CLD(name: string) return string is
		  variable rs1: std_logic_vector(4 downto 0);
		  variable rd: std_logic_vector(4 downto 0);
			variable imm: std_logic_vector(6 downto 0);
		begin																
			rs1 := get_rvc_i(insn(9 downto 7));
			rd := get_rvc_i(insn(4 downto 2));
			imm(5 downto 3) := insn(12 downto 10);
			imm(2) := insn(6);
			imm(6) := insn(5);
			imm(1 downto 0) := "00";			
			return name & " " & get_register_name(rd) & ", " & 
			  imm_toString(imm) & "(" & get_register_name(rs1) & ")";
		end function CLD;

		function CSW(name: string) return string is
		  variable rs1: std_logic_vector(4 downto 0);
		  variable rs2: std_logic_vector(4 downto 0);		
		  variable imm: std_logic_vector(6 downto 0);
		begin
			rs1 := get_rvc_i(insn(9 downto 7));
			rs2 := get_rvc_i(insn(4 downto 2));			
			imm(5 downto 3) := insn(12 downto 10);
			imm(2) := insn(6);
			imm(6) := insn(5);
			imm(1 downto 0) := "00";			
			return name & " " & get_register_name(rs2) & ", " & 
			  imm_toString(imm) & "(" & get_register_name(rs1) & ")";			
		end function CSW;	
		
		function CLWSP(name: string) return string is 
		  variable rd: std_logic_vector(4 downto 0);
		  variable imm: std_logic_vector(7 downto 0);
		begin
			rd := insn(11 downto 7);
			imm(5) := insn(12);
			imm(4 downto 2) := insn(6 downto 4);
			imm(7 downto 6) := insn(3 downto 2);
			imm(1 downto 0) := "00";
			return name & " " & get_register_name(rd) & ", " & 
			  imm_toString(imm) & "(sp)";
		end function CLWSP;	
		
		function CSWSP(name: string) return string is
		  variable rs2: std_logic_vector(4 downto 0);
		  variable imm: std_logic_vector(7 downto 0);		
		begin					
			rs2 := insn(11 downto 7);
			imm(5) := insn(12);
			imm(4 downto 2) := insn(6 downto 4);
			imm(7 downto 6) := insn(3 downto 2);
			imm(1 downto 0) := "00";						
			imm(1 downto 0) := "00";			
			return name & " " & get_register_name(rs2) & ", " & 
			  imm_toString(imm) & "(sp)";			
		end function CSWSP;	
	begin
		opcode := insn(1 downto 0);	
		funct3 := insn(15 downto 13);
		funct4 := insn(15 downto 12);
		
		case funct3 is
			when "000" =>
			  case opcode is
					when "00" =>  
					  if insn(4 downto 2) /= 0 then  -- C.ADDI4SPN
						  return CADDI4SPN;
						end if;
					when "01" =>
					  if insn(11 downto 7) /= 0 then  -- C.ADDI
							return CI("c.add", true);
						else
							if insn(12) = '0' and insn(6 downto 2) = 0 then  -- C.NOP
								return "c.nop";
							end if;	
						end if;
					when "10" => 
					  if insn(11 downto 7) /= 0 then
					    if insn(12) = '0' then	-- C.SLLI
							  return CSHI("c.sll", insn(11 downto 7));
						  end if;
						end if;	
					when others => null;
				end case;
			when "001" =>
			  case opcode is
					when "00" =>
					
					when "01" =>	-- C.JAL
						return CJAL(true);
					when "10" =>
					
					when others => null;
				end case;			
			when "010" =>
			  case opcode is
					when "00" =>	-- C.LW
					  return CLD("c.lw");
					when "01" =>	
					  if insn(11 downto 7) /= 0 then  -- C.LI
						  return CLI("c.li");
						end if;
					when "10" =>  -- C.LWSP
					  if insn(11 downto 7) /= 0 then
						  return CLWSP("c.lwsp");
						end if;
					when others => null;
				end case;			
			when "011" =>
			  case opcode is
					when "00" => 
					
					when "01" =>
					  if insn(11 downto 7) /= 0 and insn(11 downto 7) /= 2 then  -- C.LUI
						  return CLUI;
						end if;	
						if insn(11 downto 7) = 2 then  -- C.ADDI16SP
							return CADDI16SP;
						end if;	
					when "10" =>
					
					when others => null;
				end case;			
			when "100" =>
			  case opcode is
					when "00" =>

					when "01" =>
					  case insn(11 downto 10) is
							when "00" =>	
					      if insn(12) = '0' then	-- C.SRLI
									return CSHI("c.srl", get_rvc_i(insn(9 downto 7)));
						    end if;							
							when "01" =>
					      if insn(12) = '0' then	-- C.SRAI
									return CSHI("c.sra", get_rvc_i(insn(9 downto 7)));
						    end if;							
							when "10" =>  -- C.ANDI
								return CI("c.and", false);
							when "11" =>
							  if insn(12) = '0' then
									case insn(6 downto 5) is
										when "00" =>	-- C.SUB 
											return CR("c.sub");
										when "01" =>	-- C.XOR
											return CR("c.xor");
										when "10" =>	-- C.OR
											return CR("c.or");
										when "11" =>	-- C.AND
											return CR("c.and");
										when others => null;
									end case;	
								else -- insn(12) = '1'	
									case insn(6 downto 5) is
										when "00" =>	-- C.SUBW
											null;
										when "01" =>  -- C.ADDW
											null;
										when "10" =>  -- Reserved
											null;
										when "11" =>  -- Reserved
											null;
										when others => null;
									end case;									
								end if;
							when others => null;
						end case;
					when "10" =>
					  if insn(12) = '0' then
							if insn(11 downto 7) /= 0 then
								if insn(6 downto 2) /= 0 then  -- C.MV
									return MV; 
								else	-- C.JR
									return CJALR(false);
								end if;	
							end if;	
						else 	-- insn(12) = '1'
							if insn(11 downto 7) /= 0 then
								if insn(6 downto 2) /= 0 then  -- C.ADD
									return "c.add" & " " & get_register_name(insn(11 downto 7)) & ", " & get_register_name(insn(6 downto 2));
								else	-- C.JALR
									return CJALR(true);
								end if;	
							else	
								if insn(6 downto 2) = 0 then  -- C.BREAK
									return "c.break";
								end if;	
							end if;	
						end if;
					when others => null;
				end case;			
			when "101" =>
			  case opcode is
					when "00" =>
					
					when "01" =>	-- C.J
						return CJAL(false);
					when "10" =>
					
					when others => null;
				end case;			
			when "110" =>
			  case opcode is
					when "00" =>	-- C.SW
					  return CSW("c.sw");
					when "01" =>  -- C.BEQZ
						return CBR("c.breq");
					when "10" =>	-- C.SWSP 
					  return CSWSP("c.swsp");
					when others => null;
				end case;			
			when "111"=>
			  case opcode is
					when "00" =>
					
					when "01" =>	 -- C.BNEZ
						return CBR("c.bnez");
					when "10" =>
					
					when others => null;
				end case;			
			when others =>  null;
		end case;		
		
		return "???";
	end function disassemble_rv32c;	
end package body disassembler_rv32c;



