-- Project Sorcerer - RISC-V core
--
-- Instruction Decoder for basic instructions ("I" & "M")
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
use work.pipeline_iface.all;

entity decoder_rv32im is 
	generic
	(
	  ENABLE_DIV: natural range 0 to 1 := 1;
		ENABLE_A: natural range 0 to 1 := 0
	);
	port
	(
	  insn: in std_logic_vector(31 downto 0);
		pc: in std_logic_vector(31 downto 0);
		dout : out insn_decoder_out_type
	);	
end decoder_rv32im;

architecture behaviour of decoder_rv32im is
begin
  decode: process(insn, pc)
	  variable v: insn_decoder_out_type;
		variable opcode: std_logic_vector(6 downto 0);
		variable funct3: std_logic_vector(2 downto 0);
		variable funct7: std_logic_vector(6 downto 0);
		
		procedure signImm12 is
		  variable imm: std_logic_vector(11 downto 0);
		begin																					
			imm := insn(31 downto 20);
			v.immediate := sign_extend32(imm);
			v.rs2_imm := '1';
		end procedure signImm12;
		
		procedure signImm12_split is
		  variable imm: std_logic_vector(11 downto 0);
		begin																					
			imm := insn(31 downto 25) & insn(11 downto 7);
			v.immediate := sign_extend32(imm);
			v.rs2_imm := '1';
		end procedure signImm12_split;		
		
		procedure R(oper: alu_oper_type) is
		begin
			v.alu_oper := oper;
			v.rs1_rd := '1';
			v.rs2_rd := '1';
			v.reg_wr := '1';
			v.valid := '1';			
		end procedure R;
		
		procedure I(oper: alu_oper_type) is
		begin
			v.alu_oper := oper;
			v.rs1_rd := '1';														
			signImm12;
			v.reg_wr := '1';
			v.valid := '1';			
		end procedure I;
		
		procedure SHI(oper: alu_oper_type) is
		begin
			v.alu_oper := oper;
			v.rs1_rd := '1';
			v.immediate(4 downto 0) := insn(24 downto 20);
			v.rs2_imm := '1';
			v.reg_wr := '1';
			v.valid := '1';			
		end procedure SHI;
		
		procedure LD(size: size_type; sign_ext: std_logic) is
		begin
			v.alu_oper := alu_nop;
			v.rs1_rd := '1'; 
			v.mem_sign_ext := sign_ext;
			v.mem_size := size;
			v.mem_rd := '1';
			signImm12;
			v.reg_wr := '1';
			v.valid := '1';
		end procedure LD;	
		
		procedure ST(size: size_type) is
		begin
			v.alu_oper := alu_mov;
			v.rs1_rd := '1';
			v.rs2_rd := '1';
			v.mem_size := size;
			v.mem_wr := '1';
			signImm12_split;
			v.rs2_imm := '0';
			v.valid := '1';
		end procedure ST;		
		
		procedure BR(oper: branch_oper_type) is
		begin
			v.alu_oper := alu_sub;
			v.rs1_rd := '1';
			v.rs2_rd := '1';
			v.branch_oper := oper;		
			v.branch_offset(10) := insn(7);
			v.branch_offset(9 downto 4) := insn(30 downto 25);
			v.branch_offset(3 downto 0) := insn(11 downto 8);
			if insn(31) = '1' then
				v.branch_offset(v.branch_offset'high downto 11) := (others => '1');
			else	
				v.branch_offset(v.branch_offset'high downto 11) := (others => '0');
			end if;	
			v.valid := '1';
		end procedure BR;	
		
		procedure JAL is 
		begin
			v.branch_oper := br_jal;
			v.branch_offset := insn(31) & insn(19 downto 12) & insn(20) & insn(30 downto 21);
			v.reg_wr := '1';
			v.valid := '1';		
		end procedure JAL;
		
		procedure JALR is 
		begin
			v.branch_oper := br_jalr;
			v.alu_oper := alu_add;
			v.rs1_rd := '1';														
			signImm12;
			v.reg_wr := '1';
			v.valid := '1';		
		end procedure JALR;		

		procedure AMO(amoalu_oper: amoalu_oper_type) is
		begin
			LD(size32, '0');
			v.immediate := (others => '0');
			v.amoalu_oper := amoalu_oper;
			v.atomic_oper := at_amo;
		end procedure AMO;		
	begin
		v := insn_decoder_empty;
		v.dest_reg := insn(11 downto 7);
		v.csr_cmd.addr := insn(31 downto 20);
		v.csr_cmd.imm := insn(19 downto 15);
		
		opcode := insn(6 downto 0);	
		funct3 := insn(14 downto 12);
		funct7 := insn(31 downto 25);
		case opcode is
			when "0110111" =>  -- LUI
			  v.alu_oper := alu_mov2;
			  v.immediate(31 downto 12) := insn(31 downto 12);
				v.immediate(11 downto 0) := (others => '0');
			  v.rs2_imm := '1';			
				v.reg_wr := '1';
				v.valid := '1';			
			when "0010111" =>  -- AUIPC
			  v.alu_oper := alu_mov2;
			  v.immediate := pc + (insn(31 downto 12) & x"000");
			  v.rs2_imm := '1';			
				v.reg_wr := '1';
				v.valid := '1';			
			when "1101111" =>  -- JAL
				JAL;
			when "1100111" =>  -- JALR
				JALR;
			when "1100011" =>
			  case funct3 is
					when "000" =>  -- BEQ
						BR(br_eq);
					when "001" =>  -- BNE
						BR(br_ne);
					when "100" =>  -- BLT
						BR(br_lt);
					when "101" =>  -- BGE
						BR(br_ge);
					when "110" =>  -- BLTU
						BR(br_ltu);
					when "111" =>  -- BGEU
						BR(br_geu);
					when others => null;
			  end case;
			
			when "0000011" =>	
				case funct3 is
					when "000" =>  -- LB
						LD(size8, '1');
					when "001" =>  -- LH
						LD(size16, '1');
					when "010" =>   -- LW
						LD(size32, '0');
					when "100" =>  -- LBU
						LD(size8, '0');
					when "101" =>  -- LHU
						LD(size16, '0');
					when others => null;
				end case;	
				
			when "0100011" =>
			  case funct3 is
					when "000" =>  -- SB
						ST(size8);
					when "001" =>  -- SH
						ST(size16);
					when "010" =>  -- SW
						ST(size32);
					when others => null;
				end case;
				
			when "0010011" =>
			  case funct3 is
					when "000" =>  -- ADDI
					  I(alu_add);
					when "010" =>  -- SLTI
						I(alu_slt);
					when "011" =>  -- SLTIU
						I(alu_sltu);
					when "100" =>  -- XORI
						I(alu_xor);
					when "110" =>  -- ORI
						I(alu_or);
					when "111" =>  -- ANDI
						I(alu_and);
					when "001" =>  
					  case funct7 is
						  when "0000000" =>  -- SLLI
								SHI(alu_sll);
							when others => null;
						end case;
					when "101" => 
					  case funct7 is
							when "0000000" =>  -- SRLI
								SHI(alu_srl);
							when "0100000" =>  -- SRAI
								SHI(alu_sra);
					    when others => null;
					  end case;
					when others => null;
			  end case;
				
			when "0110011" =>	
			  case funct3 is 
				  when "000" =>
					  case funct7 is
					    when "0000000" =>  -- ADD
								R(alu_add);
					    when "0100000" =>  -- SUB
								R(alu_sub);
							when "0000001" =>  -- MUL
								R(alu_mul);
							when others => null;
						end case;	
					when "001" =>  
					  case funct7 is
						  when "0000000" =>  -- SLL
								R(alu_sll);
							when "0000001" =>  -- MULH
								R(alu_mulh);
							when others => null;
						end case;
					when "010" =>	
					  case funct7 is
						  when "0000000" =>  -- SLT
								R(alu_slt);
							when "0000001" =>  -- MULHSU
								R(alu_mulhsu);
							when others => null;
						end case;
					when "011" =>	
					  case funct7 is
						  when "0000000" =>  -- SLTU
								R(alu_sltu);
							when "0000001" =>  -- MULHU
								R(alu_mulhu);
							when others => null;
						end case;						
					when "100" =>	
					  case funct7 is 
						  when "0000000" =>  -- XOR
								R(alu_xor);
							when "0000001" =>  -- DIV
							  if ENABLE_DIV = 1 then
								  R(alu_div);
								end if;	
							when others => null;
						end case;
					when "101" =>
					  case funct7 is
					    when "0000000" =>  -- SRL
								R(alu_srl);
					    when "0100000" =>  -- SRA
								R(alu_sra);
							when "0000001" =>  -- DIVU
							  if ENABLE_DIV = 1 then
								  R(alu_divu);
                end if;									
							when others => null;
					  end case;
					when "110" =>	
					  case funct7 is
						  when "0000000" =>  -- OR
								R(alu_or);
							when "0000001" =>  -- REM
							  if ENABLE_DIV = 1 then
								  R(alu_rem);
								end if;	
							when others => null;
						end case;	
					when "111" =>	
					  case funct7 is
						  when "0000000" =>  -- AND
								R(alu_and);
							when "0000001" =>  -- REMU
							  if ENABLE_DIV = 1 then
								  R(alu_remu);
								end if;	
							when others => null;
						end case;						
					when others => null;
			  end case;

			when "0101111" =>	 -- RV32A Standard instructions	
			  if ENABLE_A = 1 then
				  if funct3 = "010" then
					  case insn(31 downto 27) is
							when "00010" =>  
							  if insn(24 downto 20) = "00000" then  -- LR.W
									LD(size32, '0');
									v.immediate := (others => '0');
									v.atomic_oper := at_lr;
								end if;
							when "00011" =>  -- SC.W
							  ST(size32);
								v.immediate := (others => '0');
								v.atomic_oper := at_sc;	
								v.reg_wr := '1';
							when "00001" =>  -- AMOSWAP.W
								AMO(amoalu_swap); 
							when "00000" =>  -- AMOADD.W
								AMO(amoalu_add);
							when "00100" =>  -- AMOXOR.W
								AMO(amoalu_xor);
							when "01100" =>  -- AMOAND.W
								AMO(amoalu_and);
							when "01000" =>  -- AMOOR.W
								AMO(amoalu_or);
							when "10000" =>  -- AMOMIN.W
								AMO(amoalu_min);
							when "10100" =>  -- AMOMAX.W
								AMO(amoalu_max);
							when "11000" =>  -- AMOMINU.W
								AMO(amoalu_minu);
							when "11100" =>  -- AMOMAXU.W
								AMO(amoalu_maxu);
							when others => null;
						end case;	
					end if;	
				end if;
				
			when "0001111" =>
			  case funct3 is
			    when "000" =>  
					  if insn(31 downto 28) = "0000" and insn(19 downto 15) = "00000" and
						   insn(11 downto 7) = "00000" then  -- FENCE
						  v.fence := '1';
							v.valid := '1';
						end if;	
			    when "001" =>  
					  if insn(31 downto 28) = "0000" and insn(27 downto 24) = "0000" and 
							 insn(23 downto 20) = "0000" and insn(19 downto 15) = "00000" and
							 insn(11 downto 7) = "00000" then  -- FENCE.I
						  v.fence_i := '1';
							v.valid := '1';						
						end if;
					when others => null;	
			  end case;	
				
			when "1110011" =>
			  case funct3 is							 
				  when "000" =>	
					  if insn(11 downto 7) = 0 and insn(31 downto 20) = "000100000100" then -- SFENCE.VM 
						  -- Priviliged Architecture 1.9.1
							v.csr_cmd.oper := csr_sfence_vm;
							v.valid := '1';						
						end if;
						if insn(31 downto 25) = "0001001" then  -- SFENCE.VMA	
							-- Priviliged Architecture 1.10
							v.csr_cmd.oper := csr_sfence_vm;
							v.valid := '1';
						end if;						
					  if insn(19 downto 15) = 0 and insn(11 downto 7) = 0 then
					    case insn(31 downto 20) is
					      when "000000000000" =>  -- ECALL
								  v.csr_cmd.oper := csr_call;
									v.valid := '1';
					      when "000000000001" =>  -- EBREAK
								  v.csr_cmd.oper := csr_break;
									v.valid := '1';								
								when "000000000010" =>  -- URET
								  v.csr_cmd.oper := csr_ret;
									v.valid := '1';								
								when "000100000010" =>  -- SRET
								  v.csr_cmd.oper := csr_ret;
									v.valid := '1';								
								when "001000000010" =>  -- HRET
--							  v.csr_cmd.oper := csr_ret;
--								v.valid := '1';								
								when "001100000010" =>  -- MRET
								  v.csr_cmd.oper := csr_ret;
									v.valid := '1';	
								when "011110110010" =>  -- DRET
								  v.csr_cmd.oper := csr_ret;
									v.valid := '1';								
								when "000100000101" =>  -- WFI
								  v.csr_cmd.oper := csr_wfi;
									v.valid := '1';	
							  when others => null;
					    end case;
						end if;	
					when "001" =>  -- CSRRW
					  v.csr_cmd.oper := csr_rw;
						v.csr_cmd.write := '1';
						v.rs1_rd := '1';
						v.reg_wr := '1';
						v.valid := '1';
					when "010" =>  -- CSRRS
					  v.csr_cmd.oper := csr_rs;
						if insn(19 downto 15) /= 0 then
							v.csr_cmd.write := '1';
						end if;	
						v.rs1_rd := '1';
						v.reg_wr := '1';
						v.valid := '1';					
					when "011" =>  -- CSRRC
					  v.csr_cmd.oper := csr_rc;
						if insn(19 downto 15) /= 0 then
							v.csr_cmd.write := '1';
						end if;					
						v.rs1_rd := '1';
						v.reg_wr := '1';
						v.valid := '1';					
					when "101" =>  -- CSRRWI
					  v.csr_cmd.oper := csr_rwi;
						v.csr_cmd.write := '1';
						v.csr_cmd.write := '1';
						v.reg_wr := '1';
						v.valid := '1';					
					when "110" =>  -- CSRRSI
					  v.csr_cmd.oper := csr_rsi;
						if insn(19 downto 15) /= 0 then
							v.csr_cmd.write := '1';
						end if;					
						v.reg_wr := '1';
						v.valid := '1';					
					when "111" =>  -- CSRRCI
					  v.csr_cmd.oper := csr_rci;
						if insn(19 downto 15) /= 0 then
							v.csr_cmd.write := '1';
						end if;					
						v.reg_wr := '1';
						v.valid := '1';			
			    when others => null;
			  end case;		
				
			when others => null;
		end case;	
		
		dout <= v;
	end process decode;	

end behaviour;
