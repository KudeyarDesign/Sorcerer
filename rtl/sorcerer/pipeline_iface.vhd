-- Project Sorcerer - RISC-V core
--
-- Pipelibe constants, types & functions
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
use IEEE.STD_LOGIC_ARITH.all;
use work.config.all;
use work.utils.all;
use work.cache_utils.all;

package pipeline_iface is															 
	-- Processor register indexes
	constant ZERO_IDX: natural := 0;
	constant RA_IDX: natural := 1;
	constant SP_IDX: natural := 2;
	constant GP_IDX: natural := 3;
	constant TP_IDX: natural := 4;
	constant T0_IDX: natural := 5;
	constant T1_IDX: natural := 6;
	constant T2_IDX: natural := 7;
	constant S0_IDX: natural := 8;
	constant S1_IDX: natural := 9;
	constant A0_IDX: natural := 10;
	constant A1_IDX: natural := 11;
	constant A2_IDX: natural := 12;
	constant A3_IDX: natural := 13;
	constant A4_IDX: natural := 14;
	constant A5_IDX: natural := 15;
	constant A6_IDX: natural := 16;
	constant A7_IDX: natural := 17;
	constant S2_IDX: natural := 18;
	constant S3_IDX: natural := 19;
	constant S4_IDX: natural := 20;
	constant S5_IDX: natural := 21;
	constant S6_IDX: natural := 22;
	constant S7_IDX: natural := 23;
	constant S8_IDX: natural := 24;
	constant S9_IDX: natural := 25;	
	constant S10_IDX: natural := 26;
	constant S11_IDX: natural := 27;	
	constant T3_IDX: natural := 28;
	constant T4_IDX: natural := 29;
	constant T5_IDX: natural := 30;
	constant T6_IDX: natural := 31;	

	-- Processor Registers
	subtype reg_num_type is std_logic_vector(4 downto 0);
	constant ZERO_REG: reg_num_type := CONV_STD_LOGIC_VECTOR(ZERO_IDX, 5);
	constant RA_REG: reg_num_type := CONV_STD_LOGIC_VECTOR(RA_IDX, 5);
	constant SP_REG: reg_num_type := CONV_STD_LOGIC_VECTOR(SP_IDX, 5);
	constant GP_REG: reg_num_type := CONV_STD_LOGIC_VECTOR(GP_IDX, 5); 
	constant TP_REG: reg_num_type := CONV_STD_LOGIC_VECTOR(TP_IDX, 5);
	constant T0_REG: reg_num_type := CONV_STD_LOGIC_VECTOR(T0_IDX, 5);
	constant T1_REG: reg_num_type := CONV_STD_LOGIC_VECTOR(T1_IDX, 5);
	constant T2_REG: reg_num_type := CONV_STD_LOGIC_VECTOR(T2_IDX, 5);	
	constant S0_REG: reg_num_type := CONV_STD_LOGIC_VECTOR(S0_IDX, 5);
	constant S1_REG: reg_num_type := CONV_STD_LOGIC_VECTOR(S1_IDX, 5);
	constant A0_REG: reg_num_type := CONV_STD_LOGIC_VECTOR(A0_IDX, 5);
	constant A1_REG: reg_num_type := CONV_STD_LOGIC_VECTOR(A1_IDX, 5);
	constant A2_REG: reg_num_type := CONV_STD_LOGIC_VECTOR(A2_IDX, 5);
	constant A3_REG: reg_num_type := CONV_STD_LOGIC_VECTOR(A3_IDX, 5);
	constant A4_REG: reg_num_type := CONV_STD_LOGIC_VECTOR(A4_IDX, 5);
	constant A5_REG: reg_num_type := CONV_STD_LOGIC_VECTOR(A5_IDX, 5);
	constant A6_REG: reg_num_type := CONV_STD_LOGIC_VECTOR(A6_IDX, 5);
	constant A7_REG: reg_num_type := CONV_STD_LOGIC_VECTOR(A7_IDX, 5);
	constant S2_REG: reg_num_type := CONV_STD_LOGIC_VECTOR(S2_IDX, 5);
	constant S3_REG: reg_num_type := CONV_STD_LOGIC_VECTOR(S3_IDX, 5);
	constant S4_REG: reg_num_type := CONV_STD_LOGIC_VECTOR(S4_IDX, 5);
	constant S5_REG: reg_num_type := CONV_STD_LOGIC_VECTOR(S5_IDX, 5);
	constant S6_REG: reg_num_type := CONV_STD_LOGIC_VECTOR(S6_IDX, 5);
	constant S7_REG: reg_num_type := CONV_STD_LOGIC_VECTOR(S7_IDX, 5);	
	constant S8_REG: reg_num_type := CONV_STD_LOGIC_VECTOR(S8_IDX, 5);
	constant S9_REG: reg_num_type := CONV_STD_LOGIC_VECTOR(S9_IDX, 5);	
	constant S10_REG: reg_num_type := CONV_STD_LOGIC_VECTOR(S10_IDX, 5);
	constant S11_REG: reg_num_type := CONV_STD_LOGIC_VECTOR(S11_IDX, 5);	
	constant T3_REG: reg_num_type := CONV_STD_LOGIC_VECTOR(T3_IDX, 5);
	constant T4_REG: reg_num_type := CONV_STD_LOGIC_VECTOR(T4_IDX, 5);	
	constant T5_REG: reg_num_type := CONV_STD_LOGIC_VECTOR(T5_IDX, 5);
	constant T6_REG: reg_num_type := CONV_STD_LOGIC_VECTOR(T6_IDX, 5);	
	
	-- Exceptions :	
	subtype cause_type is std_logic_vector(3 downto 0);
  -- Instruction address misaligned
  constant EXCEPTION_InstrMisalign : cause_type := x"0";
  -- Instruction access fault
  constant EXCEPTION_InstrFault    : cause_type := x"1";
  -- Illegal instruction
  constant EXCEPTION_InstrIllegal  : cause_type := x"2";
  -- Breakpoint
  constant EXCEPTION_Breakpoint    : cause_type := x"3";
  -- Load address misaligned
  constant EXCEPTION_LoadMisalign  : cause_type := x"4";
  -- Load access fault
  constant EXCEPTION_LoadFault     : cause_type := x"5";
  -- Store/AMO address misaligned
  constant EXCEPTION_StoreMisalign : cause_type := x"6";
  -- Store/AMO access fault
  constant EXCEPTION_StoreFault    : cause_type := x"7";
  -- Environment call from U-mode
  constant EXCEPTION_CallFromUmode : cause_type := x"8";
  -- Environment call from S-mode
  constant EXCEPTION_CallFromSmode : cause_type := x"9";
  -- Environment call from H-mode
  constant EXCEPTION_CallFromHmode : cause_type := x"A";
  -- Environment call from M-mode
  constant EXCEPTION_CallFromMmode : cause_type := x"B";	
	
	-- User software interrupt
	constant INT_UserSoftware : cause_type := x"0";
	-- Supervisor software interrupt
	constant INT_SupervisorSoftware : cause_type := x"1";
	-- Hypervisor software interrupt
	constant INT_HypervisorSoftware : cause_type := x"2";	
	-- Machine software interrupt
	constant INT_MachineSoftware : cause_type := x"3";
	-- User timer interrupt
	constant INT_UserTimer : cause_type := x"4";
	-- Supervisor timer interrupt
	constant INT_SupervisorTimer : cause_type := x"5";	
	-- Hypervisor timer interrupt
	constant INT_HypervisorTimer : cause_type := x"6";	
	-- Machine timer interrupt
	constant INT_MachineTimer : cause_type := x"7";	
	-- User external interrupt
	constant INT_UserExternal : cause_type := x"8";	
	-- Supervisor external interrupt
	constant INT_SupervisorExternal : cause_type := x"9";	
	-- Hypervisor external interrupt
	constant INT_HypervisorExternal : cause_type := x"A";
	-- Machine external interrupt
	constant INT_MachineExternal : cause_type := x"B";	
	-- Debug trigger cause
	constant INT_DebugTrigger : cause_type := x"C";	
	
	type except_cmd_type is record
		raised: std_logic;
		interrupt: std_logic;
		cause: cause_type;
	end record;	
	
	constant except_cmd_empty: except_cmd_type :=
	(
	  raised => '0',
		interrupt => '0',
		cause => (others => '0')
	);	
	
	type size_type is (size8, size16, size32);

  type alu_oper_type is 
	(
	  alu_nop,
	  alu_add, 
	  alu_and,
		alu_div,
		alu_divu,	
		alu_mov,
		alu_mov2,
		alu_mul,
		alu_mulh,
		alu_mulhu,
		alu_mulhsu,
		alu_or,
		alu_rem,
		alu_remu,
		alu_sll,
		alu_slt,
		alu_sltu,
		alu_sra,
		alu_srl,
    alu_sub,
		alu_xor
	); 
	
	type amoalu_oper_type is
	(		 
	  amoalu_nop,
	  amoalu_add, 
	  amoalu_and,
		amoalu_or,
		amoalu_xor,
		amoalu_swap,
		amoalu_min,
		amoalu_max,
		amoalu_minu,
		amoalu_maxu	
	);
	
	type atomic_oper_type is 
	(
	  at_nop, 
	  at_lr, 
		at_sc, 
	  at_amo
	);

	type branch_oper_type is 
	(	 
	  br_none,
		br_eq,
		br_ge,
		br_geu,
		br_lt,
		br_ltu,
		br_ne,
		br_jal,
		br_jalr
	);

	type alu_cmd_type is record
		src_rs1: std_logic_vector(31 downto 0);
		src_rs2: std_logic_vector(31 downto 0); 
		oper: alu_oper_type;
	end record;	
	
	constant alu_cmd_empty: alu_cmd_type :=
	(
	  src_rs1 => (others => '0'),
		src_rs2 => (others => '0'),
		oper => alu_nop
	);

	type csr_oper_type is
	(
	  csr_nop,
		csr_rw,
		csr_rs,
		csr_rc,
		csr_rwi,
		csr_rsi,
		csr_rci,
		csr_ret,
		csr_wfi,
		csr_call,
		csr_break,
		csr_sfence_vm
	);

	type csr_cmd_type is record
		oper: csr_oper_type;
		addr: std_logic_vector(11 downto 0);
		imm: std_logic_vector(4 downto 0);
		write: std_logic;  -- write to CSR
	end record;	
	
	constant csr_cmd_empty: csr_cmd_type :=
	(
	  oper => csr_nop,
		addr => (others => '0'),
		imm => (others => '0'),
		write => '0'
	);
	
  type insn_decoder_out_type is record	
		rs1_rd: std_logic;
		rs2_rd: std_logic;	
		pc_rd: std_logic;
		dest_reg: std_logic_vector(4 downto 0);
		reg_wr: std_logic;
		immediate: std_logic_vector(31 downto 0);	
		rs2_imm: std_logic;  -- '1' if immediate operand "rs2"
		alu_oper: alu_oper_type;
		branch_oper: branch_oper_type;
		branch_offset: std_logic_vector(19 downto 0);
		mem_sign_ext: std_logic;
		mem_size: size_type;
		mem_rd: std_logic;
		mem_wr: std_logic;
		csr_cmd: csr_cmd_type;
		fence: std_logic;
		fence_i: std_logic;					 
		atomic_oper: atomic_oper_type;
		amoalu_oper: amoalu_oper_type;
		ovf: std_logic;
		valid: std_logic;
	end record;	
	
	constant insn_decoder_empty: insn_decoder_out_type :=
	(
	  rs1_rd => '0',
		rs2_rd => '0', 
		pc_rd => '0',
		dest_reg => (others => '0'),
		reg_wr => '0',
		immediate => (others => '0'),
		rs2_imm => '0',
		alu_oper => alu_nop,
		branch_oper => br_none,
		branch_offset => (others => '0'),
		mem_sign_ext => '0',
		mem_size => size8,
		mem_rd => '0',
		mem_wr => '0',
		csr_cmd => csr_cmd_empty,
		fence => '0',
		fence_i => '0',
		atomic_oper => at_nop,
		amoalu_oper => amoalu_nop,
		ovf => '0',
		valid => '0'
	);					 

--	subtype cache_oper_type is std_logic_vector(2 downto 0);
--	constant CACHE_INDEX_INVALIATE: cache_oper_type := "000";
--	constant CACHE_INDEX_LOAD_TAG: cache_oper_type := "001";
--	constant CACHE_INDEX_STORE_TAG: cache_oper_type := "010";
--	constant CACHE_HIT_INVALIDATE: cache_oper_type := "100";
--	constant ICACHE_FILL: cache_oper_type := "101";
--	constant DCACHE_HIT_WB_INVALIDATE: cache_oper_type := "101";
--	constant DCACHE_HIT_WRITEBACK: cache_oper_type := "110";
--	constant CACHE_FETCH_AND_LOCK: cache_oper_type := "111";

--	type cache_cmd_in_type is record
--		req: std_logic;
--		oper: cache_oper_type;
--		address: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
--		tagLo: std_logic_vector(WORD_SIZE-1 downto 0);
--	end record;	
	
--	type cache_cmd_out_type is record
--		busy: std_logic;
--		tagLo: std_logic_vector(WORD_SIZE-1 downto 0);
--	end record;	
	
	subtype icache_data_type is std_logic_vector(ICACHE_FETCH_WIDTH-1 downto 0); 
  type icache_fetch_in_type is record	
		next_pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
    pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0); 
		vm_en: std_logic;
		address_stall: std_logic;
		fetch: std_logic;
		jump: std_logic; 
  end record;	
	
  type icache_fetch_out_type is record
    data: icache_data_type;
		busy: std_logic;
		ready: std_logic;	
		error: std_logic;
  end record;

  type icache_cmd_in_type	is record
		flush: std_logic;
	end record;	
	
	type icache_mmu_in_type is record
		ready: std_logic;
		paddress: std_logic_vector(PADDRESS_WIDTH-1 downto 0); 
		error: std_logic;
	end record;	
	
	type icache_mmu_out_type is record
		fetch: std_logic;
		vaddress: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
	end record;
	
	type icache_bus_in_type is record
		readdata: std_logic_vector(ICACHE_FETCH_WIDTH-1 downto 0);
		readdatavalid: std_logic;
		waitrequest: std_logic;
		timeout: std_logic;
	end record;
	
	type icache_bus_out_type is record
		address:  std_logic_vector(ICACHE_FETCH_WIDTH-1 downto 0);
		burstcount: std_logic_vector(log2(MAX_ICACHE_LINE_SIZE) downto 0);
		read: std_logic;		
	end record;	
	
	type dcache_mem_in_type is record
		next_vaddress: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		vaddress: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		vm_en: std_logic;
		rd: std_logic;
		next_wr: std_logic;
		wr: std_logic;		 
		next_wr_data: std_logic_vector(DCACHE_WIDTH-1 downto 0);
		wr_data: std_logic_vector(DCACHE_WIDTH-1 downto 0);			
		next_byteenable: std_logic_vector(DCACHE_WIDTH/8-1 downto 0);
		byteenable: std_logic_vector(DCACHE_WIDTH/8-1 downto 0);
	end record;
	
	type dcache_mem_out_type is record
		data: std_logic_vector(DCACHE_WIDTH-1 downto 0);
		busy: std_logic;
		ready: std_logic;
		error: std_logic;
	end record;	

  type dcache_cmd_in_type	is record
		flush: std_logic;
		write_back: std_logic;
		amoalu_oper: amoalu_oper_type;
	end record;	
	
	type dcache_mmu_in_type is record
		ready: std_logic;
		paddress: std_logic_vector(PADDRESS_WIDTH-1 downto 0); 
		error: std_logic;
	end record;	
	
	type dcache_mmu_out_type is record
		load: std_logic;
		store: std_logic;
		vaddress: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
	end record;		
	
	type dcache_bus_in_type is record
		readdata: std_logic_vector(DCACHE_WIDTH-1 downto 0);
		readdatavalid: std_logic;
		waitrequest: std_logic;
		lock: std_logic;
		timeout: std_logic;
	end record;
	
	type dcache_bus_out_type is record
		address:  std_logic_vector(PADDRESS_WIDTH-1 downto 0);
		burstcount: std_logic_vector(log2(MAX_DCACHE_LINE_SIZE) downto 0);
		byteenable: std_logic_vector(DCACHE_WIDTH/8-1 downto 0);
		read: std_logic;
		write: std_logic;
		writedata: std_logic_vector(DCACHE_WIDTH-1 downto 0); 
		locked: std_logic;
	end record;	
	
  type pte_type is record
	  ppn1: std_logic_vector(11 downto 0);
		ppn0: std_logic_vector(9 downto 0);
		d: std_logic;
		a: std_logic;		
		g: std_logic;
		u: std_logic;
		x: std_logic;
		w: std_logic;
		r: std_logic;
		v: std_logic;
	end record;	

	function pte_conv(pte: pte_type) return std_logic_vector;
	function pte_conv(pte_word: std_logic_vector(WORD_SIZE-1 downto 0)) return pte_type;
	
	type tlb_cache_in_type is record
		vaddress: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		asid: std_logic_vector(ASID_BITS-1 downto 0);
		enableVM: std_logic;
		load: std_logic;
		store: std_logic;
		execute: std_logic;
		flush: std_logic;
	end record;
	
	type tlb_cache_out_type is record
		paddress: std_logic_vector(PADDRESS_WIDTH-1 downto 0);
		invalid: std_logic;
		modified: std_logic;
		miss: std_logic;
		ready: std_logic;
		error: std_logic;
	end record;	
	
	type tlb_cache_req_type is record
		vpn1: std_logic_vector(9 downto 0);
		vpn0: std_logic_vector(9 downto 0);
		asid: std_logic_vector(ASID_BITS-1 downto 0);
		req: std_logic;		
	end record;	
	
	type joint_tlb_in_type is record
		vpn1: std_logic_vector(9 downto 0);
		vpn0: std_logic_vector(9 downto 0);
		asid: std_logic_vector(ASID_BITS-1 downto 0);
		flush: std_logic;
		req: std_logic;
	end record;
	
	type joint_tlb_out_type is record
		pte: pte_type;
		ready: std_logic;
	end record;	
	
	type joint_tlb_mem_out_type is record
		vpn1: std_logic_vector(9 downto 0);
		vpn0: std_logic_vector(9 downto 0);
		asid: std_logic_vector(ASID_BITS-1 downto 0);
		req: std_logic;
	end record;	
	
	type joint_tlb_mem_in_type is record
		pte: pte_type;
		ready: std_logic;	
		error: std_logic;
	end record;	

	type mmu_mem_out_type is record
		address: std_logic_vector(ADDRESS_WIDTH-1 downto 0);
		read: std_logic;
	end record;
	
	type mmu_mem_in_type is record
		readdata: std_logic_vector(WORD_SIZE-1 downto 0);
		waitrequest: std_logic;
		readdatavalid: std_logic;
		timeout: std_logic;
	end record;	
	
  type alu_in_type is record
    rs1: std_logic_vector(WORD_SIZE-1 downto 0);
    rs2: std_logic_vector(WORD_SIZE-1 downto 0);
    oper: alu_oper_type;	
  end record;
	
  type amoalu_in_type is record
    lhs: std_logic_vector(WORD_SIZE-1 downto 0);
    rhs: std_logic_vector(WORD_SIZE-1 downto 0);
    oper: amoalu_oper_type;	
  end record;	
	
	type alu_flags is record
		n: std_logic;
		z: std_logic;
		v: std_logic;
		c: std_logic;
	end record;	
	
	type alu_out_type is record
    result: std_logic_vector(WORD_SIZE-1 downto 0);
   	flags: alu_flags;
		br_flags: alu_flags;
	end record;											

	type mul_in_type is record
		rs1: std_logic_vector(31 downto 0);
		rs2: std_logic_vector(31 downto 0);
		sign1: std_logic;
		sign2: std_logic;
		stall: std_logic;
	end record;	
	
	type mul_out_type is record
	  result: std_logic_vector(63 downto 0);
	end record;	

	type div_in_type is record
		rs1: std_logic_vector(WORD_SIZE-1 downto 0);
		rs2: std_logic_vector(WORD_SIZE-1 downto 0);
		sign: std_logic;  -- '1' - signed division, '0' - unsigned division
		remain: std_logic; -- '1' - remainder result, '0' - quotinent result
		start: std_logic;
		flush: std_logic;
	end record;
	
	type div_out_type is record	
		result: std_logic_vector(WORD_SIZE-1 downto 0);  -- quotient or remainder
		ready: std_logic;
	end record;	
	
	type gpr_in_type is record
		s1_address: std_logic_vector(4 downto 0);
		s2_address: std_logic_vector(4 downto 0);
		rd_address_stall: std_logic;
		d_address: std_logic_vector(4 downto 0);
		d_data: std_logic_vector(31 downto 0);
		d_we: std_logic;	
	end record;	

	type gpr_out_type is record
		s1: std_logic_vector(31 downto 0);
		s2: std_logic_vector(31 downto 0);
	end record;	
	
	type hw_breakpoints_in_type is record
		pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		exec: std_logic;
		load: std_logic;
		store: std_logic;
		address: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		data: std_logic_vector(WORD_SIZE-1 downto 0);
		bytelane: std_logic_vector(WORD_SIZE/8-1 downto 0);
	end record;
	
	type hw_breakpoints_out_type is record
		pc_match: std_logic;
		mem_match: std_logic;
		match: std_logic;
		raise_exception_exec: std_logic;
		enter_debug_mode_exec: std_logic;
		raise_exception_mem: std_logic;
		enter_debug_mode_mem: std_logic;
		start_tracing: std_logic;
		stop_tracing: std_logic;
		emit_trace_data: std_logic;
	end record;	

	type extern_csr_in_type is record
		address: std_logic_vector(11 downto 0);
		writedata: std_logic_vector(WORD_SIZE-1 downto 0);
		read: std_logic;
		write: std_logic;
	end record;	
	
	type dbgm_bus_in_type is record
		address: std_logic_vector(16 downto 0);
    writedata: std_logic_vector(WORD_SIZE-1 downto 0);		
		chipselect: std_logic;
		write: std_logic;
		read: std_logic;
	end record;	
	
	type dbgm_bus_out_type is record
		readdata: std_logic_vector(WORD_SIZE-1 downto 0);
		waitrequest: std_logic;
		readdatavalid: std_logic;
	end record;	
	
	type trace_event_type is (trace_evt_none, trace_evt_jal, trace_evt_jalr, 
	  trace_evt_branch, trace_evt_load, trace_evt_store, trace_evt_trap, 
	  trace_evt_intr, trace_evt_csr_read, trace_evt_csr_write, trace_evt_drop,
		trace_evt_flush);
	type trace_in_type is record
		event: trace_event_type;
		pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		branch_taken: std_logic;
		branch_target: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		address: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		readdata: std_logic_vector(WORD_SIZE-1 downto 0);
		writedata: std_logic_vector(WORD_SIZE-1 downto 0);
		timestamp: std_logic_vector(63 downto 0);
		priv: std_logic_vector(1 downto 0);
		ie: std_logic;
	end record;	
	
	type trace_out_type is record
		stall: std_logic;
	end record;	
	
	type trace_cmd_in_type is record
		start: std_logic;
		stop: std_logic;
	end record;	
	
	type trace_csr_in_type is record
		address: std_logic_vector(11 downto 0);
		writedata: std_logic_vector(WORD_SIZE-1 downto 0);
		write: std_logic;
	end record;
	
	type trace_csr_out_type is record
		readdata: std_logic_vector(WORD_SIZE-1 downto 0);
	end record;	
	
	type trace_bus_out_type is record
		address: std_logic_vector(PADDRESS_WIDTH-1 downto 0);		
		burstcount: std_logic_vector(log2(TRACE_BURST_LEN) downto 0);
		write: std_logic;
		writedata: std_logic_vector(TRACE_BUS_WIDTH-1 downto 0);
	end record;
	
	type trace_bus_in_type is record
		waitrequest: std_logic;
	end record;	

	subtype bht_state_type is std_logic_vector(1 downto 0);
	constant BR_STRONG_NOT_TAKEN: bht_state_type := "00";
	constant BR_WEAK_NOT_TAKEN: bht_state_type := "01";
	constant BR_WEAK_TAKEN: bht_state_type := "10";
	constant BR_STRONG_TAKEN: bht_state_type := "11";
	
	type bht_data_type is record
		history: std_logic_vector(ceil_log2(BHT_SIZE)-1 downto 0);
		state: bht_state_type;
	end record;	

  function is_taken(state: std_logic_vector(1 downto 0)) return std_logic;	

	type bht_in_type is record
		next_pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		update: std_logic;
	end record;	
	
	type bht_update_type is record
		update: std_logic;
		pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		prediction: bht_data_type;
		branch_taken: std_logic;
		mispredict: std_logic;
	end record;	
	
	subtype btb_branch_type is std_logic_vector(1 downto 0);
	constant BT_EMPTY: btb_branch_type := "00";
	constant BT_RETURN: btb_branch_type := "01";
	constant BT_CONDITIONAL: btb_branch_type := "10";
	constant BT_UNCONDITIONAL: btb_branch_type := "11";

	type btb_in_type is record
		next_pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
	end record;	

	type btb_set_type is record
		lru: std_logic_vector(lru_width(BTB_ASSOCIATIVITY)-1 downto 0);
		hit_vec: std_logic_vector(BTB_ASSOCIATIVITY-1 downto 0);		
	end record;		
	
	type btb_out_type is record
		hit: std_logic;
		branch_taken: std_logic;
		target_pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		branch_type: btb_branch_type;
		btb_set: btb_set_type;
	end record;	
	
	type btb_update_type is record
		update: std_logic;
		pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		target_pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		branch_type: btb_branch_type;
		btb_set: btb_set_type;
	end record;	
	
	type ras_in_type is record
		push: std_logic;
		pop: std_logic;
		flush: std_logic;
		pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0);		
	end record;
	
	type ras_out_type is record
		empty: std_logic;
		top_pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0);		
	end record;	
	
	type bpred_in_type is record
		fetch: std_logic;
		next_pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0);		
	end record;	
	
	type bpred_out_type is record
		target_pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		hit: std_logic;
		bht_data: bht_data_type;
		
	end record;	
	
	type bpred_update_type is record
		update: std_logic;
		pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		target_pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		ret_pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		branch_taken: std_logic;
		mispredict: std_logic;
		insn_cond_branch: std_logic;
		insn_call: std_logic;
		insn_jump: std_logic;
		insn_return: std_logic;
		bht_data: bht_data_type;
		btb_set: btb_set_type;
	end record;	

  type ibuffer_in_type is record
		pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		icache_data: icache_data_type;
		icache_ready: std_logic;
		flush: std_logic;
		stall: std_logic;
		hard_stall: std_logic;		
	end record;		
	
	type ibuffer_out_type is record
		pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		insn: std_logic_vector(16*INSN_SLICE_NUM-1 downto 0);
		ready: std_logic_vector(INSN_SLICE_NUM-1 downto 0);
		page_fault: std_logic_vector(INSN_SLICE_NUM-1 downto 0);
		busy: std_logic;
	end record;	
	
	type fetch_type is record
		pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		insn_len: std_logic_vector(1 downto 0);
    data: std_logic_vector(16*INSN_SLICE_NUM-1 downto 0);
		ready: std_logic;
		invalid: std_logic;
	end record;		
	
	type PF_stage_type is record
		pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		ready: std_logic;
		jump: std_logic;
	end record;	
	
	-- Buffer/Decompressor stage, only with "C" extension
	type DC_stage_type is record	
		pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
    data: icache_data_type;
		ready: std_logic;	  	
	end record;	
	
	constant DC_empty: DC_stage_type :=
	(
	  pc => (others => '0'),
		data => (others => '0'),
		ready => '0'
	);
	
	type IF_ID_stage_type is record
		pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		ready16: std_logic;
		ready32: std_logic;
		page_fault0: std_logic;
		page_fault1: std_logic;
		insn: std_logic_vector(31 downto 0); 
		insn_len: std_logic_vector(1 downto 0);
		invalid: std_logic;
		rs1_address: std_logic_vector(4 downto 0);
		rs2_address: std_logic_vector(4 downto 0);
	end record;
	
	constant IF_ID_empty: IF_ID_stage_type :=
	(
	  pc => (others => '0'),
		ready16 => '0',
		ready32 => '0',
		page_fault0 => '0',
		page_fault1 => '0',
		insn => (others => '0'),
		insn_len => (others => '0'),
		invalid => '0',
		rs1_address => (others => '0'),
		rs2_address => (others => '0')
	);
	
	type ID_EX_stage_type is record
		ready: std_logic;
		pc: std_logic_vector(31 downto 0);
		insn_len: std_logic_vector(1 downto 0);
		alu_cmd: alu_cmd_type;
		branch_oper: branch_oper_type;
		offset: std_logic_vector(19 downto 0);
		immediate: std_logic_vector(WORD_SIZE-1 downto 0);
		mem_sign_ext: std_logic;
		mem_size: size_type;
		mem_rd: std_logic;
		mem_wr: std_logic;
		reg_wr: std_logic;
		dest_reg: std_logic_vector(4 downto 0);
		except_cmd: except_cmd_type;
		csr_cmd: csr_cmd_type;
		fence: std_logic;
		fence_i: std_logic;
		atomic_oper: atomic_oper_type;
		amoalu_oper: amoalu_oper_type;		
	end record;
	
	constant ID_EX_empty: ID_EX_stage_type :=
	(
	  ready => '0',
	  pc => (others => '0'),
		insn_len => (others => '0'),
	  alu_cmd => alu_cmd_empty,
		branch_oper => br_none,
		offset => (others => '0'), 
		immediate => (others => '0'),
		mem_sign_ext => '0',
		mem_size => size8,
		mem_rd => '0',
		mem_wr => '0',
		reg_wr => '0',
	  dest_reg => (others => '0'),
		except_cmd => except_cmd_empty,
		csr_cmd => csr_cmd_empty,
		fence => '0',
		fence_i => '0',
		atomic_oper => at_nop,
		amoalu_oper => amoalu_nop
	);
	
	type EX_MEM_stage_type is record
		ready: std_logic;
		pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		alu_result: std_logic_vector(WORD_SIZE-1 downto 0);
		branch_oper: branch_oper_type;
		branch_target: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		branch_taken: std_logic;
		pending: std_logic;
		mul_lo: std_logic;
		mul_hi: std_logic;
		mem_address: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		mem_wr_data: std_logic_vector(WORD_SIZE-1 downto 0);
		mem_sign_ext: std_logic;
		mem_size: size_type;
		mem_rd: std_logic;
		mem_wr: std_logic;
		mem_byteenable: std_logic_vector(3 downto 0);
		reg_wr: std_logic;
		dest_reg: std_logic_vector(4 downto 0);
		except_cmd: except_cmd_type;
		special: std_logic;
		atomic_oper: atomic_oper_type;
		amoalu_oper: amoalu_oper_type;		
	end record;
	
	constant EX_MEM_empty: EX_MEM_stage_type :=
	(	
	  ready => '0',
	  pc => (others => '0'),
		alu_result => (others => '0'),
		branch_oper => br_none,
		branch_target => (others => '0'),
		branch_taken => '0',
		pending => '0',
		mul_lo => '0',
		mul_hi => '0',
		mem_address => (others => '0'),
		mem_wr_data => (others => '0'),
		mem_sign_ext => '0',
		mem_size => size8,
		mem_rd => '0',
		mem_wr => '0',
		mem_byteenable => (others => '0'),
		reg_wr => '0',
		dest_reg => (others => '0'),
		except_cmd => except_cmd_empty,
		special => '0',
		atomic_oper => at_nop,
		amoalu_oper => amoalu_nop		
	);	
	
	type MEM_AL_stage_type is record
		ready: std_logic;
		pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0); 
		branch_oper: branch_oper_type;
		branch_target: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		branch_taken: std_logic;		
		mem_sign_ext: std_logic;
		mem_size: size_type;
		mem_rd: std_logic;
		mem_wr: std_logic;		
		mem_address: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		mem_rd_data: std_logic_vector(WORD_SIZE-1 downto 0);
		mem_wr_data: std_logic_vector(WORD_SIZE-1 downto 0); 
		reg_wr_data: std_logic_vector(WORD_SIZE-1 downto 0);		
		reg_wr: std_logic;
		dest_reg: std_logic_vector(4 downto 0);		
		except_cmd: except_cmd_type;
		special: std_logic;
		atomic_oper: atomic_oper_type;
		sc_cond: std_logic;
		load_reservation: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
	end record;
	
	constant MEM_AL_empty: MEM_AL_stage_type :=
	(	
	  ready => '0',
	  pc => (others => '0'), 
		branch_oper => br_none,
		branch_target => (others => '0'),
		branch_taken => '0',		
		mem_sign_ext => '0',
		mem_size => size8,
		mem_rd => '0', 
		mem_wr => '0',		
		mem_address => (others => '0'),
		mem_rd_data => (others => '0'),
		mem_wr_data => (others => '0'),
		reg_wr_data => (others => '0'),
		reg_wr => '0',
		dest_reg => (others => '0'),
		except_cmd => except_cmd_empty,
		special => '0',	
		atomic_oper => at_nop,
		sc_cond => '0',
		load_reservation => (others => '1')
	);	
	
	type AL_WB_stage_type is record
		ready: std_logic;
		pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		reg_wr_data: std_logic_vector(WORD_SIZE-1 downto 0);		
		reg_wr: std_logic;
		dest_reg: std_logic_vector(4 downto 0);			
	end record;	
	
	constant AL_WB_empty: AL_WB_stage_type :=
	(	
	  ready => '0',
	  pc => (others => '0'),
		reg_wr_data => (others => '0'),
		reg_wr => '0',
		dest_reg => (others => '0')
	);	
	
  function is_branch_taken(cond: branch_oper_type; flags: alu_flags) return std_logic;
	
	function get_branch_address(pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0); offset: std_logic_vector) 
	  return std_logic_vector; 
end package pipeline_iface;

package body pipeline_iface is
  function is_branch_taken(cond: branch_oper_type; flags: alu_flags) return std_logic is
	begin
		case cond is
		  when br_jal =>      return '1';
			when br_eq =>       return flags.z;
			when br_ge =>				return not flags.n;
			when br_geu =>			return not flags.c;
			when br_lt =>				return flags.n;
			when br_ltu =>			return flags.c;
			when br_ne =>				return not flags.z;
			when others => return '0';
		end case;
	end function is_branch_taken;	
	
	function get_branch_address(pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0); offset: std_logic_vector) 
	   return std_logic_vector is
		variable offset_se: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
	begin
		if offset(offset'high) = '1' then
			offset_se(offset_se'high downto offset'high+2) := (others => '1');
		else	
			offset_se(offset_se'high downto offset'high+2) := (others => '0');
		end if;	
		offset_se(offset'high+1 downto 1) := offset;
		offset_se(0) := '0';
		return pc + offset_se;
	end function get_branch_address;	
	
	function pte_conv(pte: pte_type) return std_logic_vector is
		variable v: std_logic_vector(WORD_SIZE-1 downto 0); 
	begin
		v(31 downto 20) := pte.ppn1;
		v(19 downto 10) := pte.ppn0;
		v(9 downto 8) := (others => '0');
		v(7) := pte.d;
		v(6) := pte.a;
		v(5) := pte.g;
		v(4) := pte.u;
		v(3) := pte.x;
		v(2) := pte.w;
		v(1) := pte.r;
		v(0) := pte.v;
		return v;
	end function pte_conv;	
	
	function pte_conv(pte_word: std_logic_vector(WORD_SIZE-1 downto 0)) return pte_type is
	  variable v: pte_type;
	begin
		v.ppn1 := pte_word(31 downto 20);
		v.ppn0 := pte_word(19 downto 10);
		v.d := pte_word(7);
		v.a := pte_word(6);
		v.g := pte_word(5);
		v.u := pte_word(4);
		v.x := pte_word(3);
		v.w := pte_word(2);
		v.r := pte_word(1);
		v.v := pte_word(0);
		return v;
	end function pte_conv;	
	
	function is_taken(state: std_logic_vector(1 downto 0)) return std_logic is
	begin
		if state = BR_WEAK_TAKEN or state = BR_STRONG_TAKEN then
			return '1';
		else	
			return '0';
		end if;	
	end function is_taken;	
end package body pipeline_iface;
