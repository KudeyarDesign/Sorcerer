-- Project Sorcerer - RISC-V core
--
-- Instruction decoder for "C" extension
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

entity decoder_rv32c is
	port
	(
	  insn: in std_logic_vector(15 downto 0);
		pc: in std_logic_vector(31 downto 0);
		dout : out insn_decoder_out_type
	);	
end decoder_rv32c;

architecture behaviour of decoder_rv32c is
begin
  decode: process(insn, pc)	
	  variable v: insn_decoder_out_type;
		variable opcode: std_logic_vector(1 downto 0);
		variable funct3: std_logic_vector(2 downto 0);
		variable funct4: std_logic_vector(3 downto 0);
		
		procedure signImm6 is
		  variable imm: std_logic_vector(5 downto 0);
		begin																					
			imm := insn(12) & insn(6 downto 2);
			v.immediate := sign_extend32(imm);
			v.rs2_imm := '1';
		end procedure signImm6;		
		
		procedure CR(oper: alu_oper_type) is
		begin
			v.alu_oper := oper;
			v.rs1_rd := '1';
			v.rs2_rd := '1';
			v.dest_reg := get_rvc_i(insn(9 downto 7));
			v.reg_wr := '1';
			v.valid := '1';			
		end procedure CR;
		
		procedure CI(oper: alu_oper_type) is
		begin
			v.alu_oper := oper;
			v.rs1_rd := '1';														
			signImm6;
			v.reg_wr := '1';
			v.valid := '1';			
		end procedure CI;
		
		procedure CSHI(oper: alu_oper_type; dest_reg: std_logic_vector(4 downto 0)) is
		begin
			v.alu_oper := oper;
			v.rs1_rd := '1';
			v.immediate(5) := insn(12);
			v.immediate(4 downto 0) := insn(6 downto 2);
			v.rs2_imm := '1';
			v.dest_reg := dest_reg;
			v.reg_wr := '1';
			v.valid := '1';			
		end procedure CSHI;
		
		procedure CBR(oper: branch_oper_type) is
		begin
			v.alu_oper := alu_sub;
			v.rs1_rd := '1';
			v.rs2_imm := '1';
			v.branch_oper := oper;		
			v.branch_offset(7) := insn(12);
			v.branch_offset(3 downto 2) := insn(11 downto 10);
			v.branch_offset(6 downto 5) := insn(6 downto 5);
			v.branch_offset(1 downto 0) := insn(4 downto 3);
			v.branch_offset(4) := insn(2);
			if insn(12) = '1' then
				v.branch_offset(v.branch_offset'high downto 8) := (others => '1');
			else	
				v.branch_offset(v.branch_offset'high downto 8) := (others => '0');
			end if;	
			v.valid := '1';
		end procedure CBR;
		
		procedure CJAL(link: boolean) is 
		begin
			v.branch_oper := br_jal;
			if link then
				v.dest_reg := RA_REG;
			else	
				v.dest_reg := ZERO_REG;
			end if;	
			v.branch_offset(10) := insn(12);
			v.branch_offset(3) := insn(11);
			v.branch_offset(8 downto 7) := insn(10 downto 9);	
			v.branch_offset(9) := insn(8);
			v.branch_offset(5) := insn(7);
			v.branch_offset(6) := insn(6);
			v.branch_offset(2 downto 0) := insn(5 downto 3);
			v.branch_offset(4) := insn(2);
			if insn(12) = '1' then
				v.branch_offset(v.branch_offset'high downto 11) := (others => '1');
			else	
				v.branch_offset(v.branch_offset'high downto 11) := (others => '0');
			end if;			
			v.reg_wr := '1';
			v.valid := '1';		
		end procedure CJAL;
		
		procedure CJALR(link: boolean) is 
		begin
			v.branch_oper := br_jalr;
			if link then
				v.dest_reg := RA_REG;
			else	
				v.dest_reg := ZERO_REG;
			end if;			
			v.alu_oper := alu_add;
			v.rs1_rd := '1';
			v.rs2_imm := '1';
			v.reg_wr := '1';
			v.valid := '1';		
		end procedure CJALR;			
	begin
		v := insn_decoder_empty;
		v.dest_reg := insn(11 downto 7);
		
		opcode := insn(1 downto 0);	
		funct3 := insn(15 downto 13);
		funct4 := insn(15 downto 12);
		
		case funct3 is
			when "000" =>
			  case opcode is
					when "00" =>  
					  if insn(4 downto 2) /= 0 then  -- C.ADDI4SPN
						  v.alu_oper := alu_add;
						  v.rs1_rd := '1';
							v.immediate(5 downto 4) := insn(12 downto 11);
							v.immediate(9 downto 6) := insn(10 downto 7);
							v.immediate(2) := insn(6);
							v.immediate(3) := insn(5);
							v.immediate(1 downto 0) := "00";
							v.rs2_imm := '1';
						  v.dest_reg := get_rvc_i(insn(4 downto 2));
							v.reg_wr := '1';
							v.valid := '1';
						end if;
					when "01" =>
					  if insn(11 downto 7) /= 0 then  -- C.ADDI
							CI(alu_add);
						else
							if insn(12) = '0' and insn(6 downto 2) = 0 then  -- C.NOP
								v.valid := '1';
							end if;	
						end if;
					when "10" => 
					  if insn(11 downto 7) /= 0 then
					    if insn(12) = '0' then	-- C.SLLI
							  CSHI(alu_sll, insn(11 downto 7));
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
			      v.alu_oper := alu_nop;
			      v.rs1_rd := '1'; 
						v.immediate(5 downto 3) := insn(12 downto 10);
						v.immediate(2) := insn(6);
						v.immediate(6) := insn(5);
						v.immediate(1 downto 0) := "00";
						v.rs2_imm := '1';
			      v.mem_size := size32;
			      v.mem_rd := '1';
						v.dest_reg := get_rvc_i(insn(4 downto 2));
			      v.reg_wr := '1';
			      v.valid := '1';					
					when "01" =>	
					  if insn(11 downto 7) /= 0 then  -- C.LI
			        v.alu_oper := alu_mov2;
							signImm6;
				      v.reg_wr := '1';
				      v.valid := '1';						
						end if;
					when "10" =>  -- C.LWSP
					  if insn(11 downto 7) /= 0 then
				      v.alu_oper := alu_nop;
				      v.rs1_rd := '1'; 
							v.immediate(5) := insn(12);
							v.immediate(4 downto 2) := insn(6 downto 4);
							v.immediate(7 downto 6) := insn(3 downto 2);
							v.immediate(1 downto 0) := "00";
							v.rs2_imm := '1';
				      v.mem_size := size32;
				      v.mem_rd := '1';
				      v.reg_wr := '1';
				      v.valid := '1';						
						end if;
					when others => null;
				end case;			
			when "011" =>
			  case opcode is
					when "00" => 
					
					when "01" =>
						if insn(11 downto 7) /= 0 and insn(11 downto 7) /= 2 then  -- C.LUI
			        v.alu_oper := alu_mov2;
			        v.immediate(17) := insn(12);
							v.immediate(16 downto 12) := insn(6 downto 2);
				      v.immediate(11 downto 0) := (others => '0');
			        v.rs2_imm := '1';			
				      v.reg_wr := '1';
				      v.valid := '1';							
						end if;	
						if insn(11 downto 7) = 2 then  -- C.ADDI16SP
							v.alu_oper := alu_add;
							v.immediate(9) := insn(12);
							v.immediate(4) := insn(6);
							v.immediate(6) := insn(5);
							v.immediate(8 downto 7) := insn(4 downto 3);
							v.immediate(5) := insn(2);
							v.immediate(3 downto 0) := "0000";
							if insn(12) = '1' then
								v.immediate(31 downto 10) := (others => '1');
							end if;	
							v.rs2_imm := '1';
			        v.rs2_imm := '1';			
				      v.reg_wr := '1';
				      v.valid := '1';							
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
									CSHI(alu_srl, get_rvc_i(insn(9 downto 7)));
						    end if;							
							when "01" =>
					      if insn(12) = '0' then	-- C.SRAI
									CSHI(alu_sra, get_rvc_i(insn(9 downto 7)));
						    end if;							
							when "10" =>  -- C.ANDI
								CI(alu_and);
							when "11" =>
							  if insn(12) = '0' then
									case insn(6 downto 5) is
										when "00" =>	-- C.SUB 
											CR(alu_sub);
										when "01" =>	-- C.XOR
											CR(alu_xor);
										when "10" =>	-- C.OR
											CR(alu_or);
										when "11" =>	-- C.AND
											CR(alu_and);
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
		              v.alu_oper := alu_mov;
			            v.rs2_rd := '1';
			            v.reg_wr := '1';
			            v.valid := '1';										
								else	-- C.JR
									CJALR(false);
								end if;	
							end if;	
						else 	-- insn(12) = '1'
							if insn(11 downto 7) /= 0 then
								if insn(6 downto 2) /= 0 then  -- C.ADD
									CR(alu_add);
								else	-- C.JALR
									CJALR(true);
								end if;	
							else	
								if insn(6 downto 2) = 0 then  -- C.EBREAK
								  v.csr_cmd.oper := csr_break;
									v.valid := '1';									
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
			      v.alu_oper := alu_mov;
			      v.rs1_rd := '1';
			      v.rs2_rd := '1';
						v.immediate(5 downto 3) := insn(12 downto 10);
						v.immediate(2) := insn(6);
						v.immediate(6) := insn(5);
						v.immediate(1 downto 0) := "00";
						v.rs2_imm := '1';						
			      v.mem_size := size32;
			      v.mem_wr := '1';
			      v.valid := '1';					
					when "01" =>  -- C.BEQZ
						CBR(br_eq);
					when "10" =>	-- C.SWSP 
			      v.alu_oper := alu_mov;
			      v.rs1_rd := '1';
			      v.rs2_rd := '1';
						v.immediate(5) := insn(12);
						v.immediate(4 downto 2) := insn(6 downto 4);
						v.immediate(7 downto 6) := insn(3 downto 2);
						v.immediate(1 downto 0) := "00";						
						v.rs2_imm := '1';						
			      v.mem_size := size32;
			      v.mem_wr := '1';
			      v.valid := '1';					
					when others => null;
				end case;			
			when "111"=>
			  case opcode is
					when "00" =>
					
					when "01" =>	 -- C.BNEZ
						CBR(br_ne);
					when "10" =>
					
					when others => null;
				end case;			
			when others =>  null;
		end case;	

		dout <= v;		
	end process decode;	
		
end behaviour;
