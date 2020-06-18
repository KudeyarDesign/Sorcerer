-- Project Sorcerer - RISC-V core
--
-- Instruction Disassembler for basic instructions ("I" & "M")
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
use work.pipeline_iface.all;
use work.disassembler_utils.all;

package disassembler_rv32im is
	function disassemble_rv32im(insn: std_logic_vector(31 downto 0); 
	    pc: std_logic_vector(31 downto 0)) return string;
end package disassembler_rv32im;

package body disassembler_rv32im is
	function imm_toString(imm: std_logic_vector(11 downto 0)) return string is
	  variable v: std_logic_vector(10 downto 0);
	begin
		if imm(11) = '1' then											
			v := "00000000000" - imm(10 downto 0);
			return "-" & toDecString(CONV_INTEGER(v));
		else																	
			return toDecString(CONV_INTEGER(imm));
		end if;	
	end function imm_toString;	

	function mem_offset_toString(offset: std_logic_vector(11 downto 0)) return string is
	  variable v: std_logic_vector(10 downto 0);
	begin
		if offset(offset'high) = '1' then											
			v := "00000000000" - offset(10 downto 0);
			return "-" & toDecString(CONV_INTEGER(v));
		else																	
			return toDecString(CONV_INTEGER(offset));
		end if;	
	end function mem_offset_toString;		

	function branch_offset_toString(imm: std_logic_vector(11 downto 0)) return string is
	  variable v: std_logic_vector(12 downto 0);
	begin														 
		v := imm & "0";
		if v(12) = '1' then											
			v := "0000000000000" - v;
			return "-" & toDecString(CONV_INTEGER(v));
		else																	
			return toDecString(CONV_INTEGER(v));
		end if;	
	end function branch_offset_toString;	
	
  function disassemble_rv32im(insn: std_logic_vector(31 downto 0); 
	    pc: std_logic_vector(31 downto 0)) return string	is
		variable opcode: std_logic_vector(6 downto 0);
		variable funct3: std_logic_vector(2 downto 0);
		variable funct7: std_logic_vector(6 downto 0);	
		
		function LUI return string is								
		  variable rd: std_logic_vector(4 downto 0);
		  variable imm: std_logic_vector(31 downto 0);
		begin			
			rd := insn(11 downto 7);
			imm(31 downto 12) := insn(31 downto 12);
			imm(11 downto 0) := (others => '0');
			return "lui" & " " & get_register_name(rd) & ", " & 
			  "0x" & toHexString(imm);
		end function LUI;
		
		function AUIPC return string is								
		  variable rd: std_logic_vector(4 downto 0);
		  variable imm: std_logic_vector(31 downto 0);
		begin			
			rd := insn(11 downto 7);
			imm(31 downto 12) := insn(31 downto 12);
			imm(11 downto 0) := (others => '0');
			return "auipc" & " " & get_register_name(rd) & ", " & 
			  "pc+" & "0x" & toHexString(imm);
		end function AUIPC;		
		
		function R(name: string) return string is
		  variable rs1: std_logic_vector(4 downto 0);
		  variable rs2: std_logic_vector(4 downto 0);
		  variable rd: std_logic_vector(4 downto 0);		
		begin			 
			rs1 := insn(19 downto 15);
			rs2 := insn(24 downto 20);
			rd := insn(11 downto 7);
			return name & " " & get_register_name(rd) & ", " & 
			   get_register_name(rs1) & ", " & get_register_name(rs2);
		end function R;
		
		function I(name: string) return string is
		  variable rs1: std_logic_vector(4 downto 0);
		  variable rd: std_logic_vector(4 downto 0);		
			variable imm: std_logic_vector(11 downto 0);
		begin
			rs1 := insn(19 downto 15);
			rd := insn(11 downto 7);
			imm := insn(31 downto 20);
			return name & " " & get_register_name(rd) & ", " & 
			   get_register_name(rs1) & ", " & imm_toString(imm);			
		end function I;
		
		function SHI(name: string) return string is
		  variable rs1: std_logic_vector(4 downto 0);
		  variable rd: std_logic_vector(4 downto 0);		
			variable imm: std_logic_vector(4 downto 0);		
		begin
			rs1 := insn(19 downto 15);
			rd := insn(11 downto 7);
			imm := insn(24 downto 20);
			return name & " " & get_register_name(rd) & ", " & 
			   get_register_name(rs1) & ", " & toDecString(CONV_INTEGER(imm));			
		end function SHI;
		
		function LD(name: string) return string is
		  variable rs1: std_logic_vector(4 downto 0);
		  variable rd: std_logic_vector(4 downto 0);		
			variable imm: std_logic_vector(11 downto 0);		
		begin
			rs1 := insn(19 downto 15);
			rd := insn(11 downto 7);
			imm := insn(31 downto 20);
			return name & " " & get_register_name(rd) & ", " & 
			   mem_offset_toString(imm) & "(" & get_register_name(rs1) & ")";			
		end function LD;
		
		function ST(name: string) return string is
		  variable rs1: std_logic_vector(4 downto 0);
		  variable rs2: std_logic_vector(4 downto 0);		
			variable imm: std_logic_vector(11 downto 0);		
		begin
			rs1 := insn(19 downto 15);
			rs2 := insn(24 downto 20);
			imm := insn(31 downto 25) & insn(11 downto 7);
			return name & " " & get_register_name(rs2) & ", " & 
			   mem_offset_toString(imm) & "(" & get_register_name(rs1) & ")";			
		end function ST;
		
		function BR(name: string) return string is
		  variable rs1: std_logic_vector(4 downto 0);
		  variable rs2: std_logic_vector(4 downto 0);		
			variable offset: std_logic_vector(11 downto 0);		
		begin
			rs1 := insn(19 downto 15);
			rs2 := insn(24 downto 20);
			offset(11) := insn(31);
			offset(10) := insn(7);
			offset(9 downto 4) := insn(30 downto 25);
			offset(3 downto 0) := insn(11 downto 8);
			return name & " " & get_register_name(rs1) & ", " & get_register_name(rs2) 
			   & ", " & branch_offset_toString(offset);
		end function BR;
		
		function JAL return string is
		  variable rd: std_logic_vector(4 downto 0);
			variable imm: std_logic_vector(31 downto 0);
			variable addr: std_logic_vector(31 downto 0);
		begin
			rd := insn(11 downto 7);
			if insn(31) = '1' then
				imm(31 downto 21) := (others => '1');
			else	
				imm(31 downto 21) := (others => '0');
			end if;	
			imm(20) := insn(31);
			imm(19 downto 12) := insn(19 downto 12);
			imm(11) := insn(20);
			imm(10 downto 1) := insn(30 downto 21);
			imm(0) := '0'; 
			addr := pc + imm;
			return "jal" & " " & get_register_name(rd) & ", " & toHexString(addr);
		end function JAL;
		
		function JALR return string is
			variable rs1: std_logic_vector(4 downto 0);
		  variable rd: std_logic_vector(4 downto 0);
			variable imm: std_logic_vector(11 downto 0);
		begin											
			rs1 := insn(19 downto 15);
			rd := insn(11 downto 7); 
			imm := insn(31 downto 20);
			return "jalr" & " " & get_register_name(rd) & ", " & imm_toString(imm) &
			  "(" & get_register_name(rs1) & ")";
		end function JALR;		
		
		function CSR(name: string) return string is
		  variable csr_addr: std_logic_vector(11 downto 0);
			variable rs1: std_logic_vector(4 downto 0);
		  variable rd: std_logic_vector(4 downto 0);
		begin
			csr_addr := insn(31 downto 20);
			rs1 := insn(19 downto 15);
			rd := insn(11 downto 7);			
			return name & " " & get_csr_name(csr_addr) & ", " & get_register_name(rd) & 
			  ", " & get_register_name(rs1);
		end function CSR;
		
		function CSRI(name: string) return string is
		  variable csr_addr: std_logic_vector(11 downto 0);
			variable imm: std_logic_vector(4 downto 0);
		  variable rd: std_logic_vector(4 downto 0);
		begin
			csr_addr := insn(31 downto 20);
			imm := insn(19 downto 15);
			rd := insn(11 downto 7);			
			return name & " " & get_csr_name(csr_addr) & ", " & get_register_name(rd) & 
			  ", " & toHexString(CONV_INTEGER(imm));	
		end function CSRI;

		function RVA(name: string) return string is
		  variable rs1: std_logic_vector(4 downto 0);
			variable rs2: std_logic_vector(4 downto 0);
		  variable rd: std_logic_vector(4 downto 0);		
			variable aq: std_logic;
			variable rl: std_logic;
		begin
			rs1 := insn(19 downto 15);
			rs2 := insn(24 downto 20);
			rd := insn(11 downto 7);
			aq := insn(26);
			rl := insn(25);
			if rs2 /= 0 then
			  return name & " " & get_register_name(rd) & ", " & get_register_name(rs2) & ", " & 
				       "(" & get_register_name(rs1) & ")";
			else  -- LR
			  return name & " " & get_register_name(rd) & ", " & 
				       "(" & get_register_name(rs1) & ")";				
			end if;	
		end function RVA;
		
	begin
		opcode := insn(6 downto 0);	
		funct3 := insn(14 downto 12);
		funct7 := insn(31 downto 25);
		case opcode is
			when "0110111" =>  -- LUI
				return LUI;
			when "0010111" =>  -- AUIPC
				return AUIPC;			
			when "1101111" =>  -- JAL
				return JAL;
			when "1100111" =>  -- JALR
				return JALR;
			when "1100011" =>
			  case funct3 is
					when "000" =>  -- BEQ
						return BR("beq");
					when "001" =>  -- BNE
						return BR("bne");
					when "100" =>  -- BLT
						return BR("blt");
					when "101" =>  -- BGE
						return BR("bge");
					when "110" =>  -- BLTU
						return BR("bltu");
					when "111" =>  -- BGEU
						return BR("bgeu");
					when others => null;
			  end case;
			
			when "0000011" =>	
				case funct3 is
					when "000" =>   -- LB
						return LD("lb");
					when "001" =>   -- LH
						return LD("lh");
					when "010" =>   -- LW
						return LD("lw");
					when "100" =>   -- LBU
						return LD("lbu");
					when "101" =>   -- LHU
						return LD("lhu");
					when others => null;
				end case;	
				
			when "0100011" =>
			  case funct3 is
					when "000" =>  -- SB
						return ST("sb");
					when "001" =>  -- SH
						return ST("sh");
					when "010" =>  -- SW
						return ST("sw");
					when others => null;
				end case;
				
			when "0010011" =>
			  case funct3 is
					when "000" =>  -- ADDI
					  return I("addi");
					when "010" =>  -- SLTI
						return I("slti");
					when "011" =>  -- SLTIU
						return I("sltiu");
					when "100" =>  -- XORI
						return I("xori");
					when "110" =>  -- ORI
						return I("ori");
					when "111" =>  -- ANDI
						return I("andi");
					when "001" =>  
					  case funct7 is
						  when "0000000" =>  -- SLLI
								return SHI("slli");
							when others => null;
						end case;
					when "101" => 
					  case funct7 is
							when "0000000" =>  -- SRLI
								return SHI("srli");
							when "0100000" =>  -- SRAI
								return SHI("srai");
					    when others => null;
					  end case;
					when others => null;
			  end case;
				
			when "0110011" =>	
			  case funct3 is 
				  when "000" =>
					  case funct7 is
					    when "0000000" =>  -- ADD
								return R("add");
					    when "0100000" =>  -- SUB
								return R("sub");
							when "0000001" =>  -- MUL
								return R("mul");
							when others => null;
						end case;	
					when "001" =>  
					  case funct7 is
						  when "0000000" =>  -- SLL
								return R("sll");
							when "0000001" =>  -- MULH
								return R("mulh");
							when others => null;
						end case;
					when "010" =>	
					  case funct7 is
						  when "0000000" =>  -- SLT
								return R("slt");
							when "0000001" =>  -- MULHSU
								return R("mulhsu");
							when others => null;
						end case;
					when "011" =>	
					  case funct7 is
						  when "0000000" =>  -- SLTU
								return R("sltu");
							when "0000001" =>  -- MULHU
								return R("mulhu");
							when others => null;
						end case;						
					when "100" =>	
					  case funct7 is 
						  when "0000000" =>  -- XOR
								return R("xor");
							when "0000001" =>  -- DIV
							  return R("div");
							when others => null;
						end case;
					when "101" =>
					  case funct7 is
					    when "0000000" =>  -- SRL
								return R("srl");
					    when "0100000" =>  -- SRA
								return R("sra");
							when "0000001" =>  -- DIVU
							  return R("divu");
							when others => null;
					  end case;
					when "110" =>	
					  case funct7 is
						  when "0000000" =>  -- OR
								return R("or");
							when "0000001" =>  -- REM
							  return R("rem");
							when others => null;
						end case;	
					when "111" =>	
					  case funct7 is
						  when "0000000" =>  -- AND
								return R("and");
							when "0000001" =>  -- REMU
							  return R("remu");
							when others => null;
						end case;						
					when others => null;
			  end case;

			when "0101111" =>	 -- RV32A Standard instructions
			  if funct3 = "010" then
				  case insn(31 downto 27) is
						when "00010" =>  
						  if insn(24 downto 20) = "00000" then  -- LR.W
								return RVA("lr.w");
							end if;
						when "00011" =>  -- SC.W
							return RVA("sc.w");
						when "00001" =>  -- AMOSWAP.W
							return RVA("amoswap.w");
						when "00000" =>  -- AMOADD.W
							return RVA("amoadd.w");
						when "00100" =>  -- AMOXOR.W
							return RVA("amoxor.w");
						when "01100" =>  -- AMOAND.W
							return RVA("amoand.w");
						when "01000" =>  -- AMOOR.W
							return RVA("amoor.w");
						when "10000" =>  -- AMOMIN.W
							return RVA("amomin.w");
						when "10100" =>  -- AMOMAX.W
							return RVA("amomax.w");
						when "11000" =>  -- AMOMINU.W
							return RVA("amominu.w");
						when "11100" =>  -- AMOMAXU.W
							return RVA("amomaxu.w");
						when others => null;
					end case;	
				end if;
				
			when "0001111" =>
			  case funct3 is
			    when "000" =>  
					  if insn(31 downto 28) = "0000" and insn(19 downto 15) = "00000" and
						   insn(11 downto 7) = "00000" then  -- FENCE
							return "fence";
						end if;	
			    when "001" =>  
					  if insn(31 downto 28) = "0000" and insn(27 downto 24) = "0000" and 
							 insn(23 downto 20) = "0000" and insn(19 downto 15) = "00000" and
							 insn(11 downto 7) = "00000" then  -- FENCE.I
							return "fence.i";
						end if;
					when others => null;	
			  end case;	
				
			when "1110011" =>
			  case funct3 is							 
				  when "000" =>
					  if insn(11 downto 7) = 0 and insn(31 downto 20) = "000100000100" then -- SFENCE.VM
						  -- Priviliged Architecture 1.9.1  
							return "sfence.vm" & " " & get_register_name(insn(19 downto 15));	
						end if;	
						if insn(31 downto 25) = "0001001" then  -- SFENCE.VMA	
							-- Priviliged Architecture 1.10
							return "sfence.vma" & " " & get_register_name(insn(24 downto 20)) & ", " & get_register_name(insn(19 downto 15));
						end if;	
					  if insn(19 downto 15) = 0 and insn(11 downto 7) = 0 then
					    case insn(31 downto 20) is
					      when "000000000000" =>  -- ECALL
								  return "ecall";
					      when "000000000001" =>  -- EBREAK
								  return "ebreak";
								when "000000000010" =>  -- URET
								  return "uret";
								when "000100000010" =>  -- SRET
								  return "sret";
								when "001000000010" =>  -- HRET
								  return "hret";
								when "001100000010" =>  -- MRET
								  return "mret";
								when "011110110010" =>  -- DRET
								  return "dret";
								when "000100000101" =>  -- WFI
								  return "wfi";	
							  when others => null;
					    end case;	
						end if;					
					when "001" =>  -- CSRRW
						return CSR("csrrw");					
					when "010" =>  -- CSRRS
						return CSR("csrrs");
					when "011" =>  -- CSRRC
						return CSR("csrrc");
					when "101" =>  -- CSRRWI
						return CSRI("csrrwi");
					when "110" =>  -- CSRRSI
						return CSRI("csrrsi");
					when "111" =>  -- CSRRCI
						return CSRI("csrrci");
			    when others => null;
			  end case;		
				
			when others => null;
		end case;		 
		
		return "???";
	end function disassemble_rv32im;	
	
end package body disassembler_rv32im;	