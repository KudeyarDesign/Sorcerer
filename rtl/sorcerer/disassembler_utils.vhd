-- Project Sorcerer - RISC-V core
--
-- Disassembler Utility functions
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
use STD.TEXTIO.all;
use work.config.all;
use work.utils.all;
use work.csr.all;
use work.pipeline_iface.all;

package disassembler_utils is
	function get_register_name(reg: std_logic_vector(4 downto 0)) return string;
	function get_csr_name(csr: std_logic_vector(11 downto 0)) return string;
	
  function toDecString(num: natural) return string;
	function toDecSignedString(num: std_logic_vector(31 downto 0)) return string;
	function toHexString(num: natural) return string;
	function toHexString(num: std_logic_vector(31 downto 0)) return string;
	
	procedure strcpy(variable dest: inout string; src: in string);
	procedure strcpy(variable dest: inout string; variable src: in LINE);
	
	constant UNDEF_INSTR: string := "???";
	constant UNDEF_OPERAND: string := "???";
	
	subtype instruction_string is string(1 to 39);	
	
  function get_size_suffix(size: size_type) return string;
	function alu_oper_toString(oper: alu_oper_type) return string;
	function alu_cmd_toString(alu_cmd: alu_cmd_type) return string;
	function csr_oper_toString(oper: csr_oper_type) return string;
	function amoalu_oper_toString(oper: amoalu_oper_type) return string;
	
	function ID_EX_toString(ex: ID_EX_stage_type) return string;
	function EX_MEM_toString(mem: EX_MEM_stage_type) return string;
	function MEM_AL_toString(al: MEM_AL_stage_type) return string;
	function AL_WB_toString(wb: AL_WB_stage_type) return string;	
end package disassembler_utils;

package body disassembler_utils is
  type char_array is array (0 to 15) of character;
  constant hex_array : char_array := ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');	

  procedure strcpy(variable dest: inout string; src: in string) is
	begin	
		for i in src'low to src'high loop
			dest(i+ dest'low- src'low) := src(i);
		end loop;
		for i in src'high+ 1+ dest'low- src'low to dest'high loop
			dest(i) := ' ';
		end loop;	
	end procedure strcpy;
	
	procedure strcpy(variable dest: inout string; variable src: in LINE) is
	begin
		if src = null then
		  for i in dest'low to dest'high loop
			  dest(i) := ' ';
		  end loop;			
		else	
			strcpy(dest, src.all);
		end if;	
	end procedure strcpy;	
	
	function toHexChar(a: std_logic_vector(3 downto 0)) return character is
	begin
	  case a is
		  when "0000" => return('0');
		  when "0001" => return('1');
		  when "0010" => return('2');
		  when "0011" => return('3');
		  when "0100" => return('4');
		  when "0101" => return('5');
		  when "0110" => return('6');
		  when "0111" => return('7');
		  when "1000" => return('8');
		  when "1001" => return('9');
		  when "1010" => return('A');
		  when "1011" => return('B');
		  when "1100" => return('C');
		  when "1101" => return('D');
		  when "1110" => return('E');
		  when "1111" => return('F');
		  when others => return('X');
	  end case;
	end;	

	function toDecString(num: std_logic_vector(2 downto 0)) return string is
	  variable str: string(1 downto 1);
	begin
		str(1) := toHexChar('0' & num);
		return str;
	end function toDecString;	
	
	function toDecString(num: natural) return string is
	  variable len : integer := 0;
	  variable str: string(10 downto 1);
	  variable v : integer := num;
	begin
	  for i in 0 to 9 loop 
	     str(i+ 1) := hex_array(v mod 10);
	     if str(i+ 1) /= '0'  then
	        len := i;
	     end if;
	     v := v/ 10;
	  end loop;
	  return (str(len+ 1 downto 1));
	end function toDecString;	

	function toDecSignedString(num: std_logic_vector(31 downto 0)) return string is
	  variable v: std_logic_vector(30 downto 0);
	begin
		if num(31) = '1' then					 
			v := (others => '0');
			v := v - num(30 downto 0);
			return "-" & toDecString(CONV_INTEGER(v));
		else	
			return toDecString(CONV_INTEGER(num(30 downto 0)));
		end if;	
	end function toDecSignedString;	
	
	function toHexString(num: natural) return string is
	  variable len : integer := 0;
	  variable str: string(8 downto 1);
	  variable v : natural := num;
	begin
	  for i in 0 to 7 loop 
	     str(i+ 1) := hex_array(v mod 16);
	     if str(i+ 1) /= '0'  then
	        len := i;
	     end if;
	     v := v/ 16;
	  end loop;
	  return (str(len+ 1 downto 1));
	end function toHexString;	
	
  function toHexString(num: std_logic_vector(31 downto 0)) return string is
	  variable len : integer := 0;
	  variable str: string(8 downto 1);
		variable v: natural;
	begin
	  for i in 0 to 7 loop
			v := CONV_INTEGER(num(4*i+3 downto 4*i));
			str(i+ 1) := hex_array(v);
	    if str(i+ 1) /= '0'  then
	       len := i;
	    end if;			
		end loop;
		return (str(len+ 1 downto 1));
	end function toHexString;	
	
	function get_size_suffix(size: size_type) return string is
	begin
		case size is
			when size8 =>   return ".B";
			when size16 =>  return ".H";
			when size32 =>  return ".W";
		end case;	
	end function get_size_suffix;
	
	function get_sign_ext_suffix(sign_ext: std_logic) return string is
	begin
		if sign_ext = '1' then
			return "S";
		else	
			return "";
		end if;	
	end function get_sign_ext_suffix;	
	
	function alu_oper_toString(oper: alu_oper_type) return string is
	begin
	  case oper is 			
	    when alu_nop =>   return "NOP";
		  when alu_add =>   return "ADD"; 
		  when alu_and =>   return "AND";
			when alu_div =>   return "DIV";
			when alu_divu =>  return "DIVU";	
			when alu_mov =>   return "MOV";
			when alu_mov2 =>  return "MOV2";
			when alu_mul =>   return "MUL";
			when alu_mulh =>  return "MULH";
			when alu_mulhu => return "MULHU";
			when alu_mulhsu => return "MULHSU";
			when alu_or =>    return "OR";
			when alu_rem =>   return "REM";
			when alu_remu =>  return "REMU";
			when alu_sll =>   return "SLL";
			when alu_slt =>   return "SLT"; 
			when alu_sltu =>  return "SLTU";
			when alu_sra =>   return "SRA";
			when alu_srl =>   return "SRL";
	    when alu_sub =>   return "SUB";
			when alu_xor =>   return "XOR";	
		end case;	
	end function alu_oper_toString;
	
	function amoalu_oper_toString(oper: amoalu_oper_type) return string is
	begin
	  case oper is 			
	    when amoalu_nop =>   return "AMONOP";
		  when amoalu_add =>   return "AMOADD"; 
		  when amoalu_and =>   return "AMOAND";
			when amoalu_or =>    return "AMOOR";
			when amoalu_xor =>   return "AMOXOR";	
		  when amoalu_swap =>  return "AMOSWAP";
		  when amoalu_min =>		return "AMOMIN";
		  when amoalu_max =>		return "AMOMAX";
		  when amoalu_minu =>	return "AMOMINU";
		  when amoalu_maxu	=>	return "AMOMAXU";
		end case;	
	end function amoalu_oper_toString;	
	
	function alu_cmd_toString(alu_cmd: alu_cmd_type) return string is
	begin	
		if alu_cmd.oper /= alu_nop then	
          return alu_oper_toString(alu_cmd.oper) & "(" &
		         toHexString(alu_cmd.src_rs1) & "," &
		         toHexString(alu_cmd.src_rs2) & 
		        ")";
		else
			return "";
		end if;	
	end function alu_cmd_toString;	

	function cond_branch_toString(name: string; address: std_logic_vector(VADDRESS_WIDTH-1 downto 0)) 
	    return string is
	begin
		return " " & name & "(" & toHexString(address) & ")";
	end function cond_branch_toString;	
	
	function branch_cmd_toString(ex: ID_EX_stage_type) return string	is
	  variable br_target: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
	begin
		br_target := get_branch_address(ex.pc, ex.offset);
		case ex.branch_oper is
		  when br_none =>	 return "";
			when br_eq =>		 return cond_branch_toString("BEQ", br_target);
			when br_ge =>		 return cond_branch_toString("BGE", br_target);
			when br_geu =>	 return cond_branch_toString("BGEU", br_target);
			when br_lt =>		 return cond_branch_toString("BLT", br_target);
			when br_ltu =>	 return cond_branch_toString("BLTU", br_target);
			when br_ne =>		 return cond_branch_toString("BNE", br_target);
			when br_jal =>	 return " J" & "(" & get_register_name(ex.dest_reg) & "," & toHexString(br_target) & ")";
			when br_jalr =>	 return " JALR" & "(" & get_register_name(ex.dest_reg) & ")";
		end case;
		return "";
	end function branch_cmd_toString;	
	
	function cause_toString(intr: std_logic; cause: cause_type) return string is
	begin	
		if intr = '1' then
		  case cause is
		    when INT_UserSoftware => return "UserSoftware";
		    when INT_SupervisorSoftware => return "SupervisorSoftware";
		    when INT_HypervisorSoftware => return "HypervisorSoftware";	
		    when INT_MachineSoftware => return "MachineSoftware";
		    when INT_UserTimer => return "UserTimer";
		    when INT_SupervisorTimer => return "SupervisorTimer";	
		    when INT_HypervisorTimer => return "HypervisorTimer";	
		    when INT_MachineTimer => return "MachineTimer";	
		    when INT_UserExternal => return "UserExternal";	
		    when INT_SupervisorExternal => return "SupervisorExternal";	
		    when INT_HypervisorExternal => return "HypervisorExternal";
		    when INT_MachineExternal  => return "MachineExternal";
				when INT_DebugTrigger => return "DebugTrigger";
				when others => return "???";
			end case;			
		else	
			case cause is
		    when EXCEPTION_InstrMisalign =>  return "InstrMisalign";
		    when EXCEPTION_InstrFault    =>  return "InstrFault";
		    when EXCEPTION_InstrIllegal  =>  return "InstrIllegal";
		    when EXCEPTION_Breakpoint    =>  return "Breakpoint";
		    when EXCEPTION_LoadMisalign  =>  return "LoadMisalign";
		    when EXCEPTION_LoadFault     =>  return "LoadFault";
		    when EXCEPTION_StoreMisalign =>  return "StoreMisalign";
		    when EXCEPTION_StoreFault    =>  return "StoreFault";
		    when EXCEPTION_CallFromUmode =>  return "CallFromUmode";
		    when EXCEPTION_CallFromSmode =>  return "CallFromSmode";
		    when EXCEPTION_CallFromHmode =>  return "CallFromHmode";
		    when EXCEPTION_CallFromMmode =>  return "CallFromMmode";		
				when others => return "???";
			end case;
		end if;
	end function cause_toString;
	
	function except_cmd_toString(except_cmd: except_cmd_type) return string is
	begin
		if except_cmd.raised = '1' then
			return " !" & cause_toString(except_cmd.interrupt, except_cmd.cause);
		else	
			return "";
		end if;	
	end function except_cmd_toString;	
	
  function get_register_name(reg: std_logic_vector(4 downto 0)) return string is
	begin		
		if true then
			case reg is
			  when "00000" =>  return "zero";
				when "00001" =>  return "ra";
				when "00010" =>  return "sp";
				when "00011" =>  return "gp";
				when "00100" =>  return "tp";
				when "00101" =>  return "t0";
				when "00110" =>  return "t1";
				when "00111" =>  return "t2";
				when "01000" =>  return "s0";
				when "01001" =>  return "s1";
				when "01010" =>  return "a0";
				when "01011" =>  return "a1";
				when "01100" =>  return "a2";
				when "01101" =>  return "a3";
				when "01110" =>  return "a4";
				when "01111" =>  return "a5";
				when "10000" =>  return "a6";
				when "10001" =>  return "a7";
				when "10010" =>  return "s2";
				when "10011" =>  return "s3";
				when "10100" =>  return "s4";
				when "10101" =>  return "s5";
				when "10110" =>  return "s6";
				when "10111" =>  return "s7";
				when "11000" =>  return "s8";
				when "11001" =>  return "s9";
				when "11010" =>  return "s10";
				when "11011" =>  return "s11";
				when "11100" =>  return "t3";
				when "11101" =>  return "t4";
				when "11110" =>  return "t5";
				when "11111" =>  return "t6";
				when others =>   return "???";
			end case;	
		else
			case reg is
			  when "00000" =>  return "zero";
				when "00001" =>  return "x1";
				when "00010" =>  return "x2";
				when "00011" =>  return "x3";
				when "00100" =>  return "x4";
				when "00101" =>  return "x5";
				when "00110" =>  return "x6";
				when "00111" =>  return "x7";
				when "01000" =>  return "x8";
				when "01001" =>  return "x9";
				when "01010" =>  return "x10";
				when "01011" =>  return "x11";
				when "01100" =>  return "x12";
				when "01101" =>  return "x13";
				when "01110" =>  return "x14";
				when "01111" =>  return "x15";
				when "10000" =>  return "x16";
				when "10001" =>  return "x17";
				when "10010" =>  return "x18";
				when "10011" =>  return "x19";
				when "10100" =>  return "x20";
				when "10101" =>  return "x21";
				when "10110" =>  return "x22";
				when "10111" =>  return "x23";
				when "11000" =>  return "x24";
				when "11001" =>  return "x25";
				when "11010" =>  return "x26";
				when "11011" =>  return "x27";
				when "11100" =>  return "x28";
				when "11101" =>  return "x29";
				when "11110" =>  return "x30";
				when "11111" =>  return "x31";
				when others =>   return "???";
			end case;
		end if;
	end function get_register_name;
	
  function get_csr_name(csr: std_logic_vector(11 downto 0)) return string is
	begin
		case csr is			
      when CSR_ustatus =>   	   return "ustatus";
      when CSR_uie =>						 return "uie";
      when CSR_utvec =>					 return "utvec";
      when CSR_uscratch => 			 return "uscratch";
      when CSR_uepc =>  	 			 return "uepc";
      when CSR_ucause =>				 return "ucause";
      when CSR_ubadaddr =>			 return "ubadaddr";
      when CSR_uip =>						 return "uip";
      when CSR_fflags =>				 return "fflags";
      when CSR_frm =>						 return "frm";
      when CSR_fcsr =>					 return "fcsr";
      when CSR_cycle =>					 return "cycle";
      when CSR_time =>					 return "time";
      when CSR_instret =>				 return "instret";
      when CSR_hpmcounter3 =>		 return "hpmcounter3";
      when CSR_hpmcounter4 =>		 return "hpmcounter4";
      when CSR_hpmcounter31 =>	 return "hpmcounter31";
      when CSR_cycleh =>				 return "cycleh";
      when CSR_timeh =>					 return "timeh";
      when CSR_instreth =>			 return "instreth";
      when CSR_hpmcounter3h =>	 return "hpmcounter3h";
      when CSR_hpmcounter4h =>	 return "hpmcounter4h";
      when CSR_hpmcounter31h =>	 return "hpmcounter31h";
      when CSR_sstatus =>				 return "sstatus";
      when CSR_sedeleg =>				 return "sedeleg";
      when CSR_sideleg =>				 return "sideleg";
      when CSR_sie =>						 return "sie";
      when CSR_stvec =>					 return "stvec";
      when CSR_sscratch =>			 return "sscratch";
      when CSR_sepc =>					 return "sepc";
      when CSR_scause =>				 return "scause";
      when CSR_sbadaddr =>			 return "sbadaddr";
      when CSR_sip =>						 return "sip";
      when CSR_sptbr =>					 return "sptbr";
      when CSR_hstatus =>				 return "hstatus";
      when CSR_hedeleg =>				 return "hedeleg";
      when CSR_hideleg =>				 return "hideleg";
      when CSR_hie =>						 return "hie";
      when CSR_htvec =>					 return "htvec";
      when CSR_hscratch =>			 return "hscratch";
      when CSR_hepc =>					 return "hepc";
      when CSR_hcause =>				 return "hcause";
      when CSR_hbadaddr =>			 return "hbadaddr";
      when CSR_hip =>						 return "hip";
      when CSR_TBD =>						 return "TBD";
      when CSR_mvendorid =>			 return "mvendorid";
      when CSR_marchid =>				 return "marchid";
      when CSR_mimpid =>				 return "mimpid";
      when CSR_mhartid =>				 return "mhartid";
      when CSR_mstatus =>				 return "mstatus";
      when CSR_misa =>					 return "misa";
      when CSR_medeleg =>				 return "medeleg";
      when CSR_mideleg =>				 return "mideleg";
      when CSR_mie =>						 return "mie";
      when CSR_mtvec =>					 return "mtvec";
      when CSR_mscratch =>			 return "mscratch";
      when CSR_mepc =>					 return "mepc";
      when CSR_mcause =>				 return "mcause";
      when CSR_mbadaddr =>			 return "mbadaddr";
      when CSR_mip =>						 return "mip";
      when CSR_mbase =>					 return "mbase";
      when CSR_mbound =>				 return "mbound";
      when CSR_mibase =>				 return "mibase";
      when CSR_mibound =>				 return "mibound";
      when CSR_mdbase =>				 return "mdbase";
      when CSR_mdbound =>				 return "mdbound";
      when CSR_mcycle =>				 return "mcycle";
      when CSR_minstret =>			 return "minstret";
      when CSR_mhpmcounter3 =>	 return "mhpmcounter3";
      when CSR_mhpmcounter4 =>	 return "mhpmcounter4";
      when CSR_mhpmcounter31 =>	 return "mhpmcounter31";
      when CSR_mcycleh =>				 return "mcycleh";
      when CSR_minstreth =>			 return "minstreth";
      when CSR_mhpmcounter3h =>	 return "mhpmcounter3h";
      when CSR_mhpmcounter4h =>	 return "mhpmcounter4h";
      when CSR_mhpmcounter31h => return "mhpmcounter31h";
      when CSR_mucounteren =>		 return "mucounteren";
      when CSR_mscounteren =>		 return "mscounteren";
      when CSR_mhcounteren =>		 return "mhcounteren";
      when CSR_mhpmevent3 =>		 return "mhpmevent3";
      when CSR_mhpmevent4 =>		 return "mhpmevent4";
      when CSR_mhpmevent31 =>		 return "mhpmevent31";
      when CSR_tselect =>				 return "tselect";
      when CSR_tdata1 =>				 return "tdata1";
      when CSR_tdata2 =>				 return "tdata2";
      when CSR_tdata3 =>				 return "tdata3";	 
			
      when CSR_dcsr =>					 return "dcsr";
      when CSR_dpc =>						 return "dpc";
      when CSR_dscratch	=>			 return "dscratch";
			
			when CSR_busstate =>		   return "busstate";
      when CSR_busaddress =>		 return "busaddress";
      when CSR_busdata =>				 return "busdata"; 
			when CSR_dpc0 =>           return "dpc";
      when CSR_dscratch0 =>			 return "dscratch0";
      when CSR_dscratch1 =>			 return "dscratch1";
      when CSR_ccsr =>					 return "ccsr";
      when CSR_cdtmaddress =>		 return "cdtmaddress";
			
      when CSR_bpselect =>       return "bpselect";
      when CSR_bpcontrol =>      return "bpcontrol";
      when CSR_bploaddr =>			 return "bploaddr";
      when CSR_bphiaddr =>			 return "bphiaddr";
      when CSR_bplodata =>			 return "bplodata";
      when CSR_bphidata	=>			 return "bphidata";
			
      when CSR_trace =>          return "trace";
      when CSR_tbufstart =>      return "tbufstart";
      when CSR_tbufend =>        return "tbufend";
      when CSR_tbufwrite =>      return "tbufwrite";
			
			when others =>  return "???";
		end case;	
	end function get_csr_name;	
	
	function csr_oper_toString(oper: csr_oper_type) return string is
	begin
		case oper is
		  when csr_nop =>     return "CSRNOP";
			when csr_rw =>	    return "CSRRW";
			when csr_rs =>	    return "CSRRS";
			when csr_rc =>	    return "CSRRC";
			when csr_rwi =>	    return "CSRRWI";
			when csr_rsi =>	    return "CSRRSI";
			when csr_rci =>	    return "CSRRCI";
			when csr_ret =>     return "ERET";
			when csr_wfi =>     return "WFI";
			when csr_call =>   return "ECALL";
			when csr_break	=>  return "EBREAK";
			when csr_sfence_vm => return "SFENCE.VM";
		end case;	
	end function csr_oper_toString;	
	
	function csr_cmd_toString(ex: ID_EX_stage_type) return string is
	begin
		case ex.csr_cmd.oper is
		  when csr_nop =>  return "";
			when csr_rw | csr_rs | csr_rc =>
			  return " " & csr_oper_toString(ex.csr_cmd.oper) & "(" & 
			    toHexString(CONV_INTEGER(ex.csr_cmd.addr)) & "," & toHexString(ex.alu_cmd.src_rs1) & ")";
			when csr_rwi | csr_rsi | csr_rci =>
			  return " " & csr_oper_toString(ex.csr_cmd.oper) & "(" & 
				  toHexString(CONV_INTEGER(ex.csr_cmd.addr)) & toHexString(CONV_INTEGER(ex.csr_cmd.imm)) & ")";
			when csr_ret | csr_wfi | csr_call | csr_break	=>
			  return " " & csr_oper_toString(ex.csr_cmd.oper);
			when csr_sfence_vm =>
			  return " " & csr_oper_toString(ex.csr_cmd.oper);
		end case;		
	end function csr_cmd_toString;	
	
  function ID_EX_toString(ex: ID_EX_stage_type) return string	is 
	begin		
		if ex.reg_wr = '1' then
			if ex.mem_rd = '1' then
				return get_register_name(ex.dest_reg) & " = " & "MEM" & get_size_suffix(ex.mem_size) & 
				  get_sign_ext_suffix(ex.mem_sign_ext) &
			    "[" & toHexString(ex.alu_cmd.src_rs1) & " + " & toHexString(ex.immediate) &  "]" & 
					csr_cmd_toString(ex) & branch_cmd_toString(ex) & except_cmd_toString(ex.except_cmd);
			elsif ex.branch_oper = br_jal then
				return get_register_name(ex.dest_reg) & " = " & "next_pc" & 
				  branch_cmd_toString(ex) & except_cmd_toString(ex.except_cmd);
			else	
			  return get_register_name(ex.dest_reg) & " = " & alu_cmd_toString(ex.alu_cmd) & 
				  csr_cmd_toString(ex) & branch_cmd_toString(ex) & except_cmd_toString(ex.except_cmd);
			end if;	
		elsif ex.mem_wr = '1' then
			return "MEM" & get_size_suffix(ex.mem_size) & 
			  "[" & toHexString(ex.alu_cmd.src_rs1) & " + " & toHexString(ex.immediate) & 
			  "] = " & toHexString(ex.alu_cmd.src_rs2) & csr_cmd_toString(ex) & 
				  except_cmd_toString(ex.except_cmd);
		elsif	ex.alu_cmd.oper /= alu_nop then
			return alu_cmd_toString(ex.alu_cmd) & csr_cmd_toString(ex) & branch_cmd_toString(ex) & 
			  except_cmd_toString(ex.except_cmd);
		elsif ex.fence = '1' then
			return "FENCE" & except_cmd_toString(ex.except_cmd);
		elsif ex.fence_i = '1' then	
			return "FENCE_I" & except_cmd_toString(ex.except_cmd);
		else 
			return csr_cmd_toString(ex) & branch_cmd_toString(ex) & except_cmd_toString(ex.except_cmd);
		end if;
	end function ID_EX_toString;
	
  function EX_MEM_toString(mem: EX_MEM_stage_type) return string is
	begin									
	  if mem.reg_wr = '1' then								 
			if mem.mem_rd = '1' then
				return get_register_name(mem.dest_reg) & " = " & "MEM" & get_size_suffix(mem.mem_size) & 
				  get_sign_ext_suffix(mem.mem_sign_ext) &
			    "[" & toHexString(mem.mem_address) &  "]" & 
					except_cmd_toString(mem.except_cmd);			
			else return get_register_name(mem.dest_reg) & " = " & toHexString(mem.alu_result) & 
			  except_cmd_toString(mem.except_cmd);
			end if;	
		elsif mem.mem_wr = '1' then
			return "MEM" & get_size_suffix(mem.mem_size) & 
			  "[" & toHexString(mem.mem_address) & "] = " & toHexString(mem.mem_wr_data) & 
				   except_cmd_toString(mem.except_cmd);			
		else	
			return "---"  & except_cmd_toString(mem.except_cmd);
		end if;	
	end function EX_MEM_toString;
	
  function MEM_AL_toString(al: MEM_AL_stage_type) return string	is 
	begin
		if al.reg_wr = '1' then
			return get_register_name(al.dest_reg) & " = " & toHexString(al.reg_wr_data) & 
			  except_cmd_toString(al.except_cmd);
		else	
			return "---" & except_cmd_toString(al.except_cmd);
		end if;	
	end function MEM_AL_toString;
	
  function AL_WB_toString(wb: AL_WB_stage_type) return string	is
	begin
		if wb.reg_wr = '1' then
			return get_register_name(wb.dest_reg) & " = " & toHexString(wb.reg_wr_data);
		else	
			return "---";
		end if;		
	end function AL_WB_toString;	
end package body disassembler_utils;
