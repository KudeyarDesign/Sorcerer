-- Project Sorcerer - RISC-V core
--
-- Instruction Decompressor from "C" compressed instructions to 32-bit instructions
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
use work.rv32c_utils.all;
use work.pipeline_iface.all;

entity decompressor_rv32c is
	port
	(
	  insn: in std_logic_vector(15 downto 0);
		insn32: out std_logic_vector(31 downto 0);
		valid: out std_logic
	);	
end decompressor_rv32c;

architecture behaviour of decompressor_rv32c is
begin
  decode: process(insn)	
		variable opcode: std_logic_vector(1 downto 0);
		variable funct3: std_logic_vector(2 downto 0);
		variable funct4: std_logic_vector(3 downto 0);
		variable immediate: std_logic_vector(31 downto 0);
		variable vvalid: std_logic;
		variable vinsn: std_logic_vector(31 downto 0);
		variable rs1: std_logic_vector(4 downto 0);
		variable rs2: std_logic_vector(4 downto 0);
		variable rd: std_logic_vector(4 downto 0);
		
		procedure signImm6(immediate: out std_logic_vector) is
		  variable imm: std_logic_vector(5 downto 0);
		begin																					
			imm := insn(12) & insn(6 downto 2);
			immediate := sign_extend(imm, immediate'length);
		end procedure signImm6;		

		procedure RRR(funct7: std_logic; funct3: std_logic_vector(2 downto 0);
		    rs1: std_logic_vector(4 downto 0); rs2: std_logic_vector(4 downto 0);
				rd: std_logic_vector(4 downto 0)) is
		begin
			vinsn(30) := funct7;
			vinsn(24 downto 20) := rs2;
			vinsn(19 downto 15) := rs1;	 
			vinsn(14 downto 12) := funct3;
			vinsn(11 downto 7) := rd;
			vinsn(6 downto 0) := "0110011";
		end procedure RRR;	

		procedure SHI(funct7: std_logic; funct3: std_logic_vector(2 downto 0);
		    rs1: std_logic_vector(4 downto 0); shamt: std_logic_vector(4 downto 0);
				rd: std_logic_vector(4 downto 0)) is
		begin
			vinsn(30) := funct7;
			vinsn(24 downto 20) := shamt;
			vinsn(19 downto 15) := rs1;
			vinsn(14 downto 12) := funct3;
			vinsn(11 downto 7) := rd;
			vinsn(6 downto 0) := "0010011";
		end procedure SHI;		

		procedure RRI(funct3: std_logic_vector(2 downto 0); rs1: std_logic_vector(4 downto 0); 
		    imm: std_logic_vector(11 downto 0); rd: std_logic_vector(4 downto 0)) is
		begin
			vinsn(31 downto 20) := imm;
			vinsn(19 downto 15) := rs1;
			vinsn(14 downto 12) := funct3;
			vinsn(11 downto 7) := rd;
			vinsn(6 downto 0) := "0010011";
		end procedure RRI;		
		
		procedure LUI(imm: std_logic_vector(31 downto 12); rd: std_logic_vector(4 downto 0)) is
		begin
			vinsn(31 downto 12) := imm;
			vinsn(11 downto 7) := rd;
			vinsn(6 downto 0) := "0110111";
		end procedure LUI;	
		
		procedure LD(funct3: std_logic_vector(2 downto 0); 
		    rs1: std_logic_vector(4 downto 0); imm: std_logic_vector(11 downto 0); 
		    rd: std_logic_vector(4 downto 0)) is
		begin
			vinsn(31 downto 20) := imm;
			vinsn(19 downto 15) := rs1;
			vinsn(14 downto 12) := funct3;
			vinsn(11 downto 7) := rd;
			vinsn(6 downto 0) := "0000011";			
		end procedure LD;
		
		procedure ST(funct3: std_logic_vector(2 downto 0); 
		    rs1: std_logic_vector(4 downto 0); imm: std_logic_vector(11 downto 0); 
		    rs2: std_logic_vector(4 downto 0)) is
		begin
			vinsn(31 downto 25) := imm(11 downto 5);
			vinsn(24 downto 20) := rs2;
			vinsn(19 downto 15) := rs1;
			vinsn(14 downto 12) := funct3;
			vinsn(11 downto 7) := imm(4 downto 0);
			vinsn(6 downto 0) := "0100011";			
		end procedure ST;		
		
		procedure BR(funct3: std_logic_vector(2 downto 0);
		    rs1: std_logic_vector(4 downto 0); 
		    rs2: std_logic_vector(4 downto 0);
				imm: std_logic_vector(12 downto 1)) is
		begin														
			vinsn(31) := imm(12);
			vinsn(30 downto 25) := imm(10 downto 5);
			vinsn(24 downto 20) := rs2;
			vinsn(19 downto 15) := rs1;
			vinsn(14 downto 12) := funct3;
			vinsn(11 downto 8) := imm(4 downto 1);
			vinsn(7) := imm(11);
			vinsn(6 downto 0) := "1100011";
		end procedure BR;	
		
		procedure JAL(rd: std_logic_vector(4 downto 0); imm: std_logic_vector (20 downto 1)) is
		begin
			vinsn(31) := imm(20);
			vinsn(30 downto 21) := imm(10 downto 1);
			vinsn(20) := imm(11);
			vinsn(19 downto 12) := imm(19 downto 12);
			vinsn(11 downto 7) := rd;
			vinsn(6 downto 0) := "1101111";
		end procedure JAL;		
		
		procedure JALR(rs1: std_logic_vector(4 downto 0); imm: std_logic_vector (11 downto 0); 
		    rd: std_logic_vector(4 downto 0)) is
		begin
			vinsn(31 downto 20) := imm;	
			vinsn(19 downto 15) := rs1;
			vinsn(14 downto 12) := "000";
			vinsn(11 downto 7) := rd;
			vinsn(6 downto 0) := "1100111";			
		end procedure JALR;	
		
		procedure CR(funct7: std_logic; funct3: std_logic_vector(2 downto 0); add: boolean) is
		  variable rs2: std_logic_vector(4 downto 0); 
			variable rd: std_logic_vector(4 downto 0);
		begin																
			if add then
			  rd := insn(11 downto 7);
			  rs2 := insn(6 downto 2);  
			else
			  rd := get_rvc_i(insn(9 downto 7));
			  rs2 := get_rvc_i(insn(4 downto 2)); 				
			end if;	
			RRR(funct7, funct3, rd, rs2, rd);
			vvalid := '1';			
		end procedure CR;
		
		procedure CI(funct3: std_logic_vector(2 downto 0); add: boolean) is
		  variable imm: std_logic_vector(11 downto 0);
			variable rd: std_logic_vector(4 downto 0);
		begin
			signImm6(imm);
			if add then
			  rd := insn(11 downto 7);
			else
				rd := get_rvc_i(insn(9 downto 7));
			end if;	
			RRI(funct3, rd, imm, rd);
			vvalid := '1';			
		end procedure CI;
		
		procedure CSHI(funct7: std_logic; funct3: std_logic_vector(2 downto 0)) is
		  variable nzimm: std_logic_vector(5 downto 0);
			variable rd: std_logic_vector(4 downto 0);
		begin					 
			rd := get_rvc_i(insn(9 downto 7));
			nzimm(5) := insn(12);
			nzimm(4 downto 0) := insn(6 downto 2);
			SHI(funct7, funct3, rd, nzimm(4 downto 0), rd); 
			vvalid := '1';			
		end procedure CSHI;
		
		procedure CBR(funct3: std_logic_vector(2 downto 0)) is 
		  variable rs1: std_logic_vector(4 downto 0); 
		  variable branch_offset: std_logic_vector(12 downto 1);
		begin						
			rs1 := get_rvc_i(insn(9 downto 7));
			branch_offset(8) := insn(12);
			branch_offset(4 downto 3) := insn(11 downto 10);
			branch_offset(7 downto 6) := insn(6 downto 5);
			branch_offset(2 downto 1) := insn(4 downto 3);
			branch_offset(5) := insn(2);
			if insn(12) = '1' then
				branch_offset(branch_offset'high downto 9) := (others => '1');
			else	
				branch_offset(branch_offset'high downto 9) := (others => '0');
			end if;	
			BR(funct3, rs1, "00000", branch_offset);
			vvalid := '1';
		end procedure CBR;
		
		procedure CJAL(link: boolean) is
		  variable rd: std_logic_vector(4 downto 0); 
		  variable branch_offset: std_logic_vector(20 downto 1);
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
			if insn(12) = '1' then
				branch_offset(branch_offset'high downto 11) := (others => '1');
			else	
				branch_offset(branch_offset'high downto 11) := (others => '0');
			end if;
			JAL(rd, branch_offset);
			vvalid := '1';		
		end procedure CJAL;
		
		procedure CJALR(link: boolean) is
		  variable rs1: std_logic_vector(4 downto 0);
		  variable rd: std_logic_vector(4 downto 0); 
		  variable branch_offset: std_logic_vector(20 downto 1);
		begin				
			rs1 := insn(11 downto 7);
			if link then
				rd := RA_REG;
			else	
				rd := ZERO_REG;
			end if;			
			JALR(rs1, x"000", rd);
			vvalid := '1';		
		end procedure CJALR;			
	begin
		opcode := insn(1 downto 0);	
		funct3 := insn(15 downto 13);
		funct4 := insn(15 downto 12);
		vvalid := '0';
		rs1 := (others => '0');
		rs2 := (others => '0');
		rd := (others => '0');
		immediate := (others => '0');
		vinsn := (others => '0');
		
		case funct3 is
			when "000" =>
			  case opcode is
					when "00" =>  
					  if insn(4 downto 2) /= 0 then  -- C.ADDI4SPN
							immediate(5 downto 4) := insn(12 downto 11);
							immediate(9 downto 6) := insn(10 downto 7);
							immediate(2) := insn(6);
							immediate(3) := insn(5);
							immediate(1 downto 0) := "00";
						  rd := get_rvc_i(insn(4 downto 2));
							RRI("000", SP_REG, immediate(11 downto 0), rd);
							vvalid := '1';
						end if;
					when "01" =>
					  if insn(11 downto 7) /= 0 then  -- C.ADDI
							CI("000", true);
						else
							if insn(12) = '0' and insn(6 downto 2) = 0 then  -- C.NOP
								RRI("000", ZERO_REG, x"000", ZERO_REG);
								vvalid := '1';
							end if;	
						end if;
					when "10" => 
					  if insn(11 downto 7) /= 0 then
					    if insn(12) = '0' then	-- C.SLLI
							  CSHI('0', "001");
						  end if;
						end if;	
					when others => null;
				end case;
			when "001" =>
			  case opcode is
					when "00" =>
					
					when "01" =>	-- C.JAL
						CJAL(true);
					when "10" =>
					
					when others => null;
				end case;			
			when "010" =>
			  case opcode is
					when "00" =>	-- C.LW
						immediate(5 downto 3) := insn(12 downto 10);
						immediate(2) := insn(6);
						immediate(6) := insn(5);
						immediate(1 downto 0) := "00";
						rs1 := get_rvc_i(insn(9 downto 7));
						rd := get_rvc_i(insn(4 downto 2));
						LD("010", rs1, immediate(11 downto 0), rd);
			      vvalid := '1';					
					when "01" =>	
					  if insn(11 downto 7) /= 0 then  -- C.LI
							signImm6(immediate(11 downto 0));
							rd := insn(11 downto 7);
							RRI("000", ZERO_REG, immediate(11 downto 0), rd);
				      vvalid := '1';						
						end if;
					when "10" =>  -- C.LWSP
					  if insn(11 downto 7) /= 0 then
							immediate(5) := insn(12);
							immediate(4 downto 2) := insn(6 downto 4);
							immediate(7 downto 6) := insn(3 downto 2);
							immediate(1 downto 0) := "00"; 
							rd := insn(11 downto 7);
							LD("010", SP_REG, immediate(11 downto 0), rd);
				      vvalid := '1';						
						end if;
					when others => null;
				end case;			
			when "011" =>
			  case opcode is
					when "00" => 
					
					when "01" =>
					  if insn(11 downto 7) /= 0 and insn(11 downto 7) /= 2 then  -- C.LUI
						  for i in 17 to 31 loop
			          immediate(i) := insn(12);
							end loop;	
							immediate(16 downto 12) := insn(6 downto 2);
							rd := insn(11 downto 7);
							LUI(immediate(31 downto 12), rd);
				      vvalid := '1';							
						end if;	
						if insn(11 downto 7) = 2 then  -- C.ADDI16SP
							immediate(9) := insn(12);
							immediate(4) := insn(6);
							immediate(6) := insn(5);
							immediate(8 downto 7) := insn(4 downto 3);
							immediate(5) := insn(2);
							immediate(3 downto 0) := "0000";
							if insn(12) = '1' then
								immediate(11 downto 10) := (others => '1');
							end if;	
							RRI("000", SP_REG, immediate(11 downto 0), SP_REG);
				      vvalid := '1';							
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
									CSHI('0', "101");
						    end if;							
							when "01" =>
					      if insn(12) = '0' then	-- C.SRAI
									CSHI('1', "101");
						    end if;							
							when "10" =>  -- C.ANDI
								CI("111", false);
							when "11" =>
							  if insn(12) = '0' then
									case insn(6 downto 5) is
										when "00" =>	-- C.SUB 
											CR('1', "000", false);
										when "01" =>	-- C.XOR
											CR('0', "100", false);
										when "10" =>	-- C.OR
											CR('0', "110", false);
										when "11" =>	-- C.AND
											CR('0', "111", false);
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
									rd := insn(11 downto 7);
									rs2 := insn(6 downto 2);
									RRR('0', "000", ZERO_REG, rs2, rd);
			            vvalid := '1';										
								else	-- C.JR
									CJALR(false);
								end if;	
							end if;	
						else 	-- insn(12) = '1'
							if insn(11 downto 7) /= 0 then
								if insn(6 downto 2) /= 0 then  -- C.ADD
									CR('0', "000", true);
								else	-- C.JALR
									CJALR(true);
								end if;	
							else	
								if insn(6 downto 2) = 0 then  -- C.EBREAK
								  vinsn := x"001" & "00000" & "000" & "00000" & "1110011";
									vvalid := '1';									
								end if;	
							end if;	
						end if;
					when others => null;
				end case;			
			when "101" =>
			  case opcode is
					when "00" =>
					
					when "01" =>	-- C.J
						CJAL(false);
					when "10" =>
					
					when others => null;
				end case;			
			when "110" =>
			  case opcode is
					when "00" =>	-- C.SW
						immediate(5 downto 3) := insn(12 downto 10);
						immediate(2) := insn(6);
						immediate(6) := insn(5);
						immediate(1 downto 0) := "00";							
						rs1 := get_rvc_i(insn(9 downto 7));
						rs2 := get_rvc_i(insn(4 downto 2));						
						ST("010", rs1, immediate(11 downto 0), rs2);
			      vvalid := '1';					
					when "01" =>  -- C.BEQZ
						CBR("000");
					when "10" =>	-- C.SWSP 
						immediate(5 downto 2) := insn(12 downto 9);
						immediate(7 downto 6) := insn(8 downto 7);
						immediate(1 downto 0) := "00";						
						rs2 := insn(6 downto 2);
						ST("010", SP_REG, immediate(11 downto 0), rs2);
			      vvalid := '1';					
					when others => null;
				end case;			
			when "111"=>
			  case opcode is
					when "00" =>
					
					when "01" =>	 -- C.BNEZ
						CBR("001");
					when "10" =>
					
					when others => null;
				end case;			
			when others =>  null;
		end case;	

		insn32 <= vinsn;
		valid <= vvalid;
	end process decode;

end behaviour;
