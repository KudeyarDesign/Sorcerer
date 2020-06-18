-- Project Sorcerer - RISC-V core
--
-- Main Processor Core for Avalon MM Bus
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
use work.csr.all;
use work.pipeline_iface.all; 
use work.iface.all;
use work.disassembler_utils.all;
use work.disassembler_rv32im.all;
use work.disassembler_rv32c.all;

entity sorcerer is
	generic
	(	
	  ENABLE_A: natural range 0 to 1 := 0;  -- Atomic extension
		ENABLE_C: natural range 0 to 1 := 0;  -- Compressed extension
		ENABLE_D: natural range 0 to 1 := 0;	-- Double-precision floating-point extension
		ENABLE_E: natural range 0 to 1 := 0;  -- RV32E base ISA
		ENABLE_F: natural range 0 to 1 := 0;  -- Single-precision floating-point extension
		ENABLE_G: natural range 0 to 1 := 0;  -- Additional standard extensions present
		ENABLE_M: natural range 0 to 1 := 1;	-- Integer Multiply/Divide extension
		ENABLE_N: natural range 0 to 1 := 0;	-- User-level interrupts supported
		ENABLE_S: natural range 0 to 1 := 0;	-- Supervisor mode implemented
		ENABLE_U: natural range 0 to 1 := 0;	-- User mode implemented
		ENABLE_X: natural range 0 to 1 := 0;	-- Non-standard extensions present

		ENABLE_SHIFTER: natural range 0 to 1 := 0; -- Enable barrel shifter, otherwise shifts ara implemented by use of multiplier
	  ENABLE_MMU: natural range 0 to 1 := 0;		 -- Enable MMU
		ENABLE_CACHE: natural range 0 to 1 := 1;	 -- Enable instruction and data caches
		ENABLE_DEBUG: natural range 0 to 1 := 1;   -- Enable Debug Mode
		ENABLE_HW_BREAKPOINT: natural range 0 to 1 := 0;  -- Enable Hardware Breakpoint Module 
		ENABLE_DEBUG_ROM1: natural range 0 to 1 := 1;  -- Enable Debug ROM version 1
		ENABLE_DEBUG_ROM2: natural range 0 to 1 := 0;  -- Enable Debug ROM version 2 (with bus registers)
		ENABLE_TRACE: natural range 0 to 1 := 0;   -- Enable trace events
		
		ENABLE_MISA: natural range 0 to 1 := 0;      -- Machine ISA Register
		ENABLE_MCYCLE: natural range 0 to 1 := 0;    -- Machine cycle counter
		ENABLE_MINSTRET: natural range 0 to 1 := 0;  -- Machine instructions-retired counter.
		ENABLE_BADADDR: natural range 0 to 1 := 1;   -- Bad address registers (for all enabled modes)
			
		-- MMU parameters (if MMU present, ENABLE_MMU = 1)	
	  ITLB_CACHE_SIZE: natural range 1 to 8 := 4;	   -- Instruction TLB Cache size
		DTLB_CACHE_SIZE: natural range 1 to 8 := 4;	   -- Data TLB Cache size
	  JOINT_TLB_SIZE: natural range 16 to 64 := 32;	 -- Joint (Instruction & Data) TLB size		
			
--	  DEVICE_FAMILY: string := "Cyclone III";
	  RAM_SIZE: natural range 1024 to 65536 := 16384;
		RAM_START_ADDRESS: std_logic_vector(PADDRESS_WIDTH-1 downto 0) := START_ADDRESS;
		RAM_INIT_FILE: string := "";
		
	  INSN_BP_NUM: natural range 1 to 16 := 4;  -- Number of instruction breakpoints
		DATA_BP_NUM: natural range 1 to 16 := 2;  -- Number of data breakpoints		
		
	  TRACE_FIFO_SIZE: natural range 128 to 65536 := 2048; -- Number of bytes
		TRACE_FIFO_WIDTH: natural range 32 to 512 := 128;		 -- Number of bits
		TRACE_INIT_BUF_START: integer := 0;
		TRACE_INIT_BUF_END: integer	:= 0		
	);
  port
  (
	  clk: in std_logic;
		reset: in std_logic;

		halt: std_logic := '0';
		
		mtip: in std_logic := '0';  -- External interrupt request from interrupt controller
		meip: in std_logic := '0';  -- External interrupt request from timer (machine mode)
		msip: in std_logic := '0';  -- External interrupt request from software (machine mode)
		dbg_irq: in std_logic := '0';  -- External interrupt to Debug Mode
		
		-- Diagnostic outputs
		pc: out std_logic_vector(31 downto 0);
		pc_valid: out std_logic;
		
		-- Avalon Bus Master Core Interface
		avm_mem_address: out std_logic_vector(BUS_ADDRESS_WIDTH-1 downto 0);
		avm_mem_burstcount: out std_logic_vector(log2(imax(MAX_ICACHE_LINE_SIZE, MAX_DCACHE_LINE_SIZE)) downto 0);
		avm_mem_read: out std_logic;
		avm_mem_readdata: in std_logic_vector(BUS_WIDTH-1 downto 0) := (others => '0');
		avm_mem_readdatavalid: in std_logic := '0';
		avm_mem_write: out std_logic;
		avm_mem_writedata: out std_logic_vector(BUS_WIDTH-1 downto 0);
		avm_mem_byteenable: out std_logic_vector(BUS_WIDTH/8 -1 downto 0);
--		avm_mem_lock: out std_logic;
		avm_mem_waitrequest: in std_logic := '0';
		
		-- Avalon Bus Master Trace Module Interface
    avm_trace_address: out std_logic_vector(BUS_ADDRESS_WIDTH-1 downto 0) := (others => '0');
    avm_trace_write: out std_logic := '0';
    avm_trace_writedata: out std_logic_vector(TRACE_BUS_WIDTH-1 downto 0) := (others => '0');	
	  avm_trace_burstcount: out std_logic_vector(log2(TRACE_BURST_LEN) downto 0) := (others => '0');
    avm_trace_byteenable: out std_logic_vector(TRACE_BUS_WIDTH/8 -1 downto 0) := (others => '0');
    avm_trace_waitrequest: in std_logic := '0';

		-- Avalon Bus Slave Debug Module
		avs_dbgm_address: in std_logic_vector(16-log2(WORD_SIZE/8) downto 0);
		avs_dbgm_chipselect: in std_logic;
		avs_dbgm_write: in std_logic;
		avs_dbgm_writedata: in std_logic_vector(WORD_SIZE-1 downto 0);
		avs_dbgm_read: in std_logic;
		avs_dbgm_readdata: out std_logic_vector(WORD_SIZE-1 downto 0);
		avs_dbgm_readdatavalid: out std_logic;
    avs_dbgm_waitrequest: out std_logic := '0'		
  );		
end sorcerer;

architecture behaviour of sorcerer is
  for all : gpr use entity work.gpr(altera);

	signal icache_in: icache_fetch_in_type;
	signal icache_out: icache_fetch_out_type;	
	signal icache_cmd_in: icache_cmd_in_type;
	signal icache_mmu_in: icache_mmu_in_type;
	signal icache_mmu_out: icache_mmu_out_type;
	signal icache_bus_in: icache_bus_in_type;
	signal icache_bus_out: icache_bus_out_type;	

	signal dcache_in: dcache_mem_in_type;
	signal dcache_out: dcache_mem_out_type;
	signal dcache_cmd_in: dcache_cmd_in_type;
	signal dcache_mmu_in: dcache_mmu_in_type;
	signal dcache_mmu_out: dcache_mmu_out_type;
	signal dcache_bus_in: dcache_bus_in_type;
	signal dcache_bus_out: dcache_bus_out_type;

	signal mmu_access_except: std_logic;
	signal mmu_mem_out: mmu_mem_out_type;
	signal mmu_mem_in: mmu_mem_in_type;
	signal tlb_icache_in: tlb_cache_in_type;
	signal tlb_icache_out: tlb_cache_out_type;
	signal tlb_dcache_in: tlb_cache_in_type;
	signal tlb_dcache_out: tlb_cache_out_type;	

	signal ibuffer_in: ibuffer_in_type;
	signal ibuffer_out: ibuffer_out_type;
	signal dc_insn32: std_logic_vector(31 downto 0);
	signal dc_valid: std_logic;
	signal if_out: fetch_type;
	signal decoder32_out: insn_decoder_out_type;
	signal gpr_in: gpr_in_type;
	signal gpr_out: gpr_out_type;
	signal alu_in: alu_in_type;
	signal alu_out: alu_out_type;	
	signal mul_in: mul_in_type;
	signal mul_out: mul_out_type;
	signal div_in: div_in_type;
	signal div_out: div_out_type;
	
	signal hw_bp_in: hw_breakpoints_in_type;
	signal hw_bp_out: hw_breakpoints_out_type;
	signal hw_bp_csr_in: extern_csr_in_type;
	signal hw_bp_csr_readdata: std_logic_vector(WORD_SIZE-1 downto 0);

	signal dbgm_bus_in: dbgm_bus_in_type;
	signal dbgm_bus_out: dbgm_bus_out_type;
	
	signal trace_in: trace_in_type;
	signal trace_out: trace_out_type;
	signal trace_cmd_in: trace_cmd_in_type;
	signal trace_csr_in: trace_csr_in_type;
	signal trace_csr_out: trace_csr_out_type;
	signal trace_bus_in: trace_bus_in_type;
	signal trace_bus_out: trace_bus_out_type;	
	
	signal PF, next_PF: PF_stage_type;
	signal DC, next_DC: DC_stage_type;  -- Buffer/Decompressor stage, only with "C" extension
	signal IF_ID, next_IF_ID: IF_ID_stage_type;
	signal ID_EX, next_ID_EX: ID_EX_stage_type;
	signal EX_MEM, next_EX_MEM: EX_MEM_stage_type;
	signal MEM_AL, next_MEM_AL: MEM_AL_stage_type;
	signal AL_WB, next_AL_WB: AL_WB_stage_type;

	type csr_in_type is record
		rd_addr: std_logic_vector(11 downto 0);
		wr_addr: std_logic_vector(11 downto 0);
		wr_data: std_logic_vector(WORD_SIZE-1 downto 0);
		rd: std_logic;
		wr: std_logic;
		eret: std_logic;
		dret: std_logic;
		priv: std_logic_vector(1 downto 0);
		flush_tlb: std_logic;
	end record;
	
	signal CSR_regs, next_CSR_regs: CSR_regs_type;
	signal csr_in: csr_in_type;
	signal csr_rd_data, csr_rd_data_reg: std_logic_vector(WORD_SIZE-1 downto 0);
	signal tlb_flush: std_logic;
	signal dcache_write_back: std_logic;
	alias debug_mode is CSR_regs.dcsr.debug; 
	signal ex_result: std_logic_vector(WORD_SIZE-1 downto 0);
	
	type jump_cmd_type is record
  	branch: std_logic;
	  jump: std_logic;
		irq: std_logic;		-- '1' if interrupt or trap
		intr: std_logic;  -- '1' if interrupt, '0' if trap
		irq_address: std_logic_vector(31 downto 0); 
		jump_any: std_logic;
	end record;	
	signal jump_cmd: jump_cmd_type;

	type pipeline_control_type is record
	  PF: std_logic;
		DC: std_logic;
		IFF: std_logic;
		ID: std_logic;
		EX: std_logic;
		MEM: std_logic;
		AL: std_logic;
	end record;
	
	signal stall: pipeline_control_type := ( PF => '0', DC => '0', IFF => '0', ID => '0', EX => '0', MEM => '0', AL => '0');
	signal flush: pipeline_control_type := ( PF => '0', DC => '0', IFF => '0', ID => '0', EX => '0', MEM => '0', AL => '0');	
	signal ex_pending: std_logic;
	
-- synthesis translate_off 	
  type disasm_type is record
		DC: instruction_string;
		IF_ID: instruction_string;
		ID_EX: instruction_string;
		EX_MEM: instruction_string;
		MEM_AL: instruction_string;
		AL_WB: instruction_string;
	end record;	
	
	signal disasm: disasm_type;
-- synthesis translate_on	

  function get_supported_irq_mask return std_logic_vector is
 	  variable v: CSR_ip_type;
	begin
		v := csr_ip_init;
		v.MEIP := '1';  -- External interrupt
		v.MTIP := '1';	-- Timer interrupt
		v.MSIP := '1';  -- Software interrupt
--		if ENABLE_H = 1 then
--		  v.HEIP := '1';
--		  v.HTIP := '1';
--		  v.HSIP := '1';			
--		end if;	
		if ENABLE_S = 1 then
		  v.SEIP := '1';
		  v.STIP := '1';
		  v.SSIP := '1';			
		end if;
		if ENABLE_U = 1 then
		  v.UEIP := '1';
		  v.UTIP := '1';
		  v.USIP := '1';			
		end if;		
		return CSR_get_ip(v);
	end function get_supported_irq_mask;
	
  function get_delegable_irq_mask return std_logic_vector is
 	  variable v: CSR_ip_type;
	begin
		v := csr_ip_init;
--		if ENABLE_H = 1 then
--		  v.HEIP := '1';
--		  v.HTIP := '1';
--		  v.HSIP := '1';			
--		end if;	
		if ENABLE_S = 1 then
		  v.SEIP := '1';
		  v.STIP := '1';
		  v.SSIP := '1';			
		end if;
		if ENABLE_U = 1 then
		  v.UEIP := '1';
		  v.UTIP := '1';
		  v.USIP := '1';			
		end if;		
		return CSR_get_ip(v);
	end function get_delegable_irq_mask;
	
	function get_delegable_except_mask return std_logic_vector is
	  variable v: std_logic_vector(WORD_SIZE-1 downto 0);
	begin
		v := (others => '0');
    v(CONV_INTEGER(EXCEPTION_InstrMisalign)) := '1'; 
    v(CONV_INTEGER(EXCEPTION_InstrFault)) := '1';
    v(CONV_INTEGER(EXCEPTION_Breakpoint)) := '1';
    v(CONV_INTEGER(EXCEPTION_LoadFault)) := '1';
    v(CONV_INTEGER(EXCEPTION_StoreFault)) := '1';
    v(CONV_INTEGER(EXCEPTION_CallFromUmode)) := '1';
		return v;
	end function get_delegable_except_mask;	
	
	function legalize_privilege(priv: std_logic_vector(1 downto 0)) 
	    return std_logic_vector is
	begin
		if ENABLE_MMU = 1 then
			if priv = PRV_H then
			  return PRV_U;	
			else	
				return priv;
			end if;	
		elsif ENABLE_U = 1 then
			return priv(0) & priv(0);
		else	
			return PRV_M;
		end if;
	end function legalize_privilege;	
		
  function trim_privilege(priv: std_logic_vector(1 downto 0))
	    return std_logic_vector is
	begin
	  if ENABLE_MMU = 1 then
			return priv;
	  else 
			return legalize_privilege(priv);
		end if;	
	end function trim_privilege;
	
	function get_ie(CSR_regs: CSR_regs_type; priv: std_logic_vector(1 downto 0)) return std_logic is
	  variable ie: std_logic;
	begin										 
		ie := '0';
    case CSR_regs.mode is
			when PRV_U =>  
			  if ENABLE_U = 1 then
			    return CSR_regs.mstatus.UIE;
				end if;	
			when PRV_S =>	 
			  if ENABLE_S = 1 then
			    return CSR_regs.mstatus.SIE;
				end if;	
			when PRV_H =>	 
--				  if ENABLE_H = 1 then
--				    return CSR_regs.mstatus.HIE;
--					end if;				
      when PRV_M =>	 
        return CSR_regs.mstatus.MIE;
			when others => null;
		end case;
		return ie;
	end function get_ie;
	
	function validate_csr(address: std_logic_vector(11 downto 0); priv: std_logic_vector(1 downto 0);
	    write: std_logic)  return std_logic is
		variable csr_priv: std_logic_vector(1 downto 0);
		variable csr_rd_only: std_logic;
	begin	 
		if ENABLE_S = 1 or ENABLE_U = 1 then
			csr_priv := address(9 downto 8);
			if address(11 downto 10) = "11" then
			  csr_rd_only := '1';
			else
				csr_rd_only := '0';
			end if;	
			if (write and csr_rd_only) = '1' or priv < csr_priv then
				return '0';
			else	
				return '1';
			end if;	
		else
			return '1';
		end if;	
	end function validate_csr;	
begin
	icache_in.next_pc <= next_PF.pc; -- when stall.PF = '0' else PF.pc;
  icache_in.pc <= PF.pc;
	icache_in.vm_en <= '1' when CSR_regs.mstatus.VM(3) = '1' and CSR_regs.mode /= PRV_M else '0';
	icache_in.address_stall <= '0';
	icache_in.fetch <= PF.ready;
	icache_in.jump <= jump_cmd.jump_any;	
	icache_cmd_in.flush <= ID_EX.fence_i;

	dcache_in.next_vaddress <= next_EX_MEM.mem_address when stall.MEM = '0' else EX_MEM.mem_address; 
	dcache_in.vaddress <= EX_MEM.mem_address;	
	dcache_in.vm_en <= '1' when CSR_regs.mstatus.VM(3) = '1' and CSR_regs.mode /= PRV_M else '0';
	dcache_in.rd <= EX_MEM.mem_rd;
	dcache_in.next_wr_data <= next_EX_MEM.mem_wr_data;
	dcache_in.wr_data <= EX_MEM.mem_wr_data;
	dcache_in.next_wr <= next_EX_MEM.mem_wr;
	dcache_in.wr <= EX_MEM.mem_wr;
	dcache_in.next_byteenable <= next_EX_MEM.mem_byteenable;
	dcache_in.byteenable <= EX_MEM.mem_byteenable;		
	dcache_cmd_in.flush <= '0';
	dcache_cmd_in.write_back <= dcache_write_back; 
	dcache_cmd_in.amoalu_oper <= EX_MEM.amoalu_oper;
	
-- synthesis translate_off	
	disam_gen: if ENABLE_DISASSEMBLER generate
		disasm_dc: if ENABLE_C = 1 generate
		DC_disasm: process(ibuffer_out)
		  variable v: instruction_string;
		begin
			if ibuffer_out.ready(0) = '1'	then
				strcpy(v, disassemble_rv32c(ibuffer_out.insn(15 downto 0), ibuffer_out.pc));
			else	
				strcpy(v, "---");
			end if;	
			disasm.DC <= v;
		end process DC_disasm;	
		end generate;	
		
		IF_ID_disasm: process (IF_ID)
		  variable v: instruction_string;
		begin			
			if IF_ID.ready32 = '1' then
			  strcpy(v, disassemble_rv32im(IF_ID.insn, IF_ID.pc));
			else
				strcpy(v, "---");
			end if;		
			disasm.IF_ID <= v;
		end process IF_ID_disasm;	

		ID_EX_disasm: process(ID_EX)
		  variable v: instruction_string;
		begin
			strcpy(v, ID_EX_toString(ID_EX));
			disasm.ID_EX <= v;
		end process ID_EX_disasm;		

		EX_MEM_disasm: process(EX_MEM)
		  variable v: instruction_string;
		begin
			strcpy(v, EX_MEM_toString(EX_MEM));
			disasm.EX_MEM <= v;
		end process EX_MEM_disasm;
		
		MEM_AL_disasm: process(MEM_AL)
		  variable v: instruction_string;
		begin
			strcpy(v, MEM_AL_toString(MEM_AL));
			disasm.MEM_AL <= v;
		end process MEM_AL_disasm;
		
		AL_WB_disasm: process(AL_WB)
		  variable v: instruction_string;
		begin
			strcpy(v, AL_WB_toString(AL_WB));
			disasm.AL_WB <= v;
		end process AL_WB_disasm;			
		
	end generate;
-- synthesis translate_on	
	
	min_config: if ENABLE_CACHE = 0 and ENABLE_MMU = 0 generate  -- Minimal configuration
	  bus_iface1: ram_avalon
		generic map
		(									 
		  DEVICE_FAMILY => DEVICE_FAMILY,
		  RAM_SIZE => RAM_SIZE,
			RAM_START_ADDRESS => RAM_START_ADDRESS,
			RAM_INIT_FILE => RAM_INIT_FILE
		)
		port map
		(
		  clk => clk,
			reset => reset,
			icache_in => icache_in,
			icache_out => icache_out,
			dcache_in => dcache_in,
			dcache_out => dcache_out,
			
			avm_mem_address => avm_mem_address,
			avm_mem_burstcount => avm_mem_burstcount,
			avm_mem_read => avm_mem_read,
			avm_mem_readdata => avm_mem_readdata,
			avm_mem_readdatavalid => avm_mem_readdatavalid,
			avm_mem_write => avm_mem_write,
			avm_mem_writedata => avm_mem_writedata,
			avm_mem_byteenable => avm_mem_byteenable,
			avm_mem_waitrequest => avm_mem_waitrequest
		);		
		
	end generate;	

	std_config: if ENABLE_CACHE = 1 generate  -- Standard configuration
		
	icache1: icache 
	generic map
	(
	  SIZE => ICACHE_SIZE,
		ASSOCIATIVITY => ICACHE_ASSOCIATIVITY,
		LINE_SIZE => ICACHE_LINE_SIZE,
		ENABLE_MMU => ENABLE_MMU,
		SYNTH => 1
	)
  port map
  (
	  clk => clk,
		reset => reset,
		din => icache_in,
		dout => icache_out,
		cmd_in => icache_cmd_in,
		mmu_in => icache_mmu_in,
		mmu_out => icache_mmu_out,
		bus_in => icache_bus_in,
		bus_out => icache_bus_out
	);		
	
	dcache1: dcache 
	generic map
	(
	  SIZE => DCACHE_SIZE,
		ASSOCIATIVITY => DCACHE_ASSOCIATIVITY,
		LINE_SIZE => DCACHE_LINE_SIZE,
		ENABLE_MMU => ENABLE_MMU,
		ENABLE_A => ENABLE_A,
		SYNTH => 1
	)
  port map
  (
	  clk => clk,
		reset => reset,
		din => dcache_in,
		dout => dcache_out,	
		cmd_in => dcache_cmd_in,
		mmu_in => dcache_mmu_in,
		mmu_out => dcache_mmu_out,
		bus_in => dcache_bus_in,
		bus_out => dcache_bus_out
	);	
	
	mmu_en1: if ENABLE_MMU = 1 generate
	mmu1: mmu	
	generic	map
	(
	  ITLB_CACHE_SIZE => ITLB_CACHE_SIZE,
		DTLB_CACHE_SIZE => DTLB_CACHE_SIZE,
	  JOINT_TLB_SIZE => JOINT_TLB_SIZE,
		JOINT_TLB_HASH_SIZE => JOINT_TLB_SIZE*4
	)
  port map
  (
	  clk => clk,
		reset => reset,
		
		sptbr => CSR_regs.sptbr,
		mstatus => CSR_regs.mstatus,
		debug_mode => debug_mode,
		access_except => mmu_access_except,
		
		itlb_in => tlb_icache_in,
		itlb_out => tlb_icache_out,
		dtlb_in => tlb_dcache_in,
		dtlb_out => tlb_dcache_out,
		mem_out => mmu_mem_out,
		mem_in => mmu_mem_in
	);	
	
	tlb_icache_in.vaddress <= icache_mmu_out.vaddress;
	tlb_icache_in.asid <= (others => '0');
	tlb_icache_in.enableVM <= '1';
	tlb_icache_in.load <= '0';
	tlb_icache_in.store <= '0';
	tlb_icache_in.execute <= icache_mmu_out.fetch;
	tlb_icache_in.flush <= tlb_flush; 

	icache_mmu_in.paddress <= tlb_icache_out.paddress;
	icache_mmu_in.ready <= tlb_icache_out.ready; 
	icache_mmu_in.error <= tlb_icache_out.error;
	
	tlb_dcache_in.vaddress <= dcache_mmu_out.vaddress;
	tlb_dcache_in.asid <= (others => '0');
	tlb_dcache_in.enableVM <= '1';
	tlb_dcache_in.load <= dcache_mmu_out.load;
	tlb_dcache_in.store <= dcache_mmu_out.store;
	tlb_dcache_in.execute <= '0';
	tlb_dcache_in.flush <= tlb_flush;	
	
	dcache_mmu_in.paddress <= tlb_dcache_out.paddress;
	dcache_mmu_in.ready <= tlb_dcache_out.ready;	
	dcache_mmu_in.error <= tlb_dcache_out.error;
	
	end generate;  -- ENABLE_MMU -> MMU	
	
	avm1: avalon_master	 
	generic map
	(
	  ENABLE_MMU => ENABLE_MMU,
	  TIMEOUT => BUS_TIMEOUT
	)
  port map
  (
	  clk => clk,
		reset => reset,
		icache_in => icache_bus_out,
		icache_out => icache_bus_in,
		dcache_in => dcache_bus_out,
		dcache_out => dcache_bus_in,
		mmu_in => mmu_mem_out,
		mmu_out => mmu_mem_in,
		
		avm_mem_address => avm_mem_address,
		avm_mem_burstcount => avm_mem_burstcount,
		avm_mem_read => avm_mem_read,
		avm_mem_readdata => avm_mem_readdata,
		avm_mem_readdatavalid => avm_mem_readdatavalid,
		avm_mem_write => avm_mem_write,
		avm_mem_writedata => avm_mem_writedata,
		avm_mem_byteenable => avm_mem_byteenable,	
--		avm_mem_lock => avm_mem_lock,
		avm_mem_waitrequest => avm_mem_waitrequest
	);	
	
	end generate;  -- ENABLE_CACHE -> ICache & Dcache	
	
	dc32: decoder_rv32im
	generic map
	(
	  ENABLE_DIV => ENABLE_M,
		ENABLE_A => ENABLE_A
	)
	port map
	(
	  insn => IF_ID.insn,
		pc => IF_ID.pc,
		dout => decoder32_out
	);

	regs: gpr
	port map
	(
	  clk => clk,
	  reset => reset,
		din => gpr_in,
		dout => gpr_out	
	);	
	
	gpr_in.s1_address <= next_IF_ID.rs1_address;
	gpr_in.s2_address <= next_IF_ID.rs2_address;
	gpr_in.rd_address_stall <= stall.ID;
	gpr_in.d_we <= next_AL_WB.reg_wr;
	gpr_in.d_address <= next_AL_WB.dest_reg;
	gpr_in.d_data <= next_AL_WB.reg_wr_data;	
	
	alu1: alu	
	generic map
	(
	  ENABLE_SHIFTER => ENABLE_SHIFTER
	)
	port map
	(
	  din => alu_in,
		dout => alu_out
	);	
	
	alu_in.rs1 <= ID_EX.alu_cmd.src_rs1;
	alu_in.rs2 <= ID_EX.alu_cmd.src_rs2;
	alu_in.oper <= ID_EX.alu_cmd.oper;	
	
	mul_div1: if ENABLE_M = 1 generate
	mul1: mul32x32
	port map
	(
	  clk => clk,
		reset => reset,
		din => mul_in,
		dout => mul_out
	);
	
	mul_in.stall <= '0';	
	
	div32c: div32x32
	port map
	(
	  clk => clk,
		reset => reset,
		din => div_in,
	  dout => div_out
	); 
	
	div_in.rs1 <= ID_EX.alu_cmd.src_rs1;
	div_in.rs2 <= ID_EX.alu_cmd.src_rs2;
  div_in.sign <= '1' when ID_EX.alu_cmd.oper = alu_div or ID_EX.alu_cmd.oper = alu_rem else '0';
	div_in.remain <= '1' when ID_EX.alu_cmd.oper = alu_rem or ID_EX.alu_cmd.oper = alu_remu else '0';	
	div_in.start <= '1' when ID_EX.alu_cmd.oper = alu_div or ID_EX.alu_cmd.oper = alu_divu or
	  											 ID_EX.alu_cmd.oper = alu_rem or ID_EX.alu_cmd.oper = alu_remu 
	  else '0';	
	div_in.flush <= flush.EX;	
	end generate;	
	
	flush.PF <= jump_cmd.irq;
	flush.IFF <= jump_cmd.jump_any;	-- PF.jump 
	flush.ID <= jump_cmd.jump_any;	-- PF.jump
	flush.EX <= jump_cmd.irq;
	flush.MEM <= jump_cmd.irq;
	flush.AL <= jump_cmd.irq;			
	
	jump_cmd.jump_any <= next_PF.jump;
	
	PF_stage: process(PF, ID_EX, jump_cmd, alu_out, stall, flush)
	  variable v: PF_stage_type;
		variable br_target: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		variable br_taken: std_logic;
	begin	
		v := PF;
		v.ready := '1';
		jump_cmd.branch <= '0';
		jump_cmd.jump <= '0';
		
		br_target := get_branch_address(ID_EX.pc, ID_EX.offset);
		br_taken := is_branch_taken(ID_EX.branch_oper, alu_out.br_flags);
		
		if jump_cmd.irq = '1' then
			v.pc := jump_cmd.irq_address;
		elsif ID_EX.branch_oper = br_jal then
			v.pc := br_target;
			jump_cmd.jump <= '1';
		elsif ID_EX.branch_oper = br_jalr then
			v.pc := alu_out.result; 
			jump_cmd.jump <= '1';
		elsif br_taken = '1' then
			v.pc := br_target;
			jump_cmd.branch <= '1';			
		else
			if stall.PF = '0' then
		    if PF.ready = '1' then
		      v.pc(31 downto 2) := PF.pc(31 downto 2) + 1; -- TODO 
				  v.pc(1 downto 0) := "00";
		    end if;	 
			end if;	
		end if;
		
		if flush.PF = '1' then
			v.ready := '0';
		end if;
		v.jump := jump_cmd.irq or jump_cmd.branch or jump_cmd.jump;
		next_PF <= v;
	end process PF_stage;			
	
	dc_yes: if ENABLE_C = 1 generate
		
  ibuf: ibuffer
  generic map
  (
	  BUFFER_DEPTH => 4,
		UNREGISTERED_OUT => 1
  )
  port map
  (
	  clk => clk,
	  reset => reset,
	  din => ibuffer_in,
	  dout => ibuffer_out
  );
	
	ibuffer_in.pc <= DC.pc;
	ibuffer_in.icache_data <= DC.data;
	ibuffer_in.icache_ready <= DC.ready;
	ibuffer_in.flush <= flush.IFF;
	ibuffer_in.stall <= stall.IFF;
	ibuffer_in.hard_stall <= '0';
	
	decompressor: decompressor_rv32c
	port map
	(
    insn => ibuffer_out.insn(15 downto 0),
	  insn32 => dc_insn32,
	  valid => dc_valid
	);
		
	DC_stage: process(DC, icache_out, ibuffer_out, dc_insn32, dc_valid)
	  variable v: DC_stage_type;
	  variable f: fetch_type;
	begin
	  v.pc := PF.pc;
	  v.data := icache_out.data;
	  v.ready := icache_out.ready; 		
		if ibuffer_out.insn(1 downto 0) = "11" then  -- >= 32-bit instruction
		  f.data := ibuffer_out.insn;
			f.ready := ibuffer_out.ready(1);
			f.insn_len := "10";
			f.invalid := '0';
		else	-- 16-bit instruction, needs decompression
		  f.data(31 downto 0) := dc_insn32;
			f.ready := ibuffer_out.ready(0);
			f.insn_len := "01";
			f.invalid := f.ready and not dc_valid;
		end if;	
		f.PC := ibuffer_out.pc;
		
		if_out <= f;
		next_DC <= v;
	end process DC_stage;	
	
	stall.DC <= ibuffer_out.busy;
	stall.PF <= (stall.DC or ((PF.ready and not icache_out.ready and not jump_cmd.jump_any))) and not flush.PF;
	end generate;	
	
	dc_no: if ENABLE_C = 0 generate
		if_out.pc <= PF.pc;
		if_out.insn_len <= "10";
		if_out.data <= icache_out.data;
		if_out.ready <= icache_out.ready;
		if_out.invalid <= '0';
		stall.PF <= ((PF.ready and not icache_out.ready and not jump_cmd.jump_any) or stall.IFF) and not flush.PF;
	end generate;	

	IF_stage: process(if_out, IF_ID, jump_cmd, stall, flush)
	  variable v: IF_ID_stage_type;
		variable extend_lo, extend_hi: std_logic;
		variable sel_hi: std_logic;
		variable vstall: std_logic;	
	begin
		v.pc := if_out.pc; 
		v.insn_len := if_out.insn_len;
	  v.ready32 := if_out.ready and not jump_cmd.branch;
		v.ready16 := '0';
		v.page_fault0 := '0';  -- TODO
		v.page_fault1 := '0';  -- TODO
	  v.insn := if_out.data;
		v.invalid := if_out.invalid;
		v.rs1_address := v.insn(19 downto 15);
		v.rs2_address := v.insn(24 downto 20);
		if flush.IFF = '1' then
		  v.ready32 := '0';
			v.ready16 := '0';
		end if;	
		next_IF_ID <= v; 
		stall.IFF <= stall.ID and not flush.IFF;
	end process IF_stage;	

	ID_stage: process(IF_ID, ID_EX, ex_result, ex_pending, EX_MEM, MEM_AL, next_AL_WB, 
	    decoder32_out, gpr_out, alu_out, jump_cmd, stall, flush)
	  variable v: ID_EX_stage_type;
		variable ready: std_logic;
		variable rs1: std_logic_vector(4 downto 0);
		variable rs2: std_logic_vector(4 downto 0);
		variable dc: insn_decoder_out_type;	
		variable operand_stall: std_logic;
		
		procedure bubble is
		begin
			v.ready := '0';
		  v.mem_rd := '0';
		  v.mem_wr := '0';		
		  v.reg_wr := '0';
			v.branch_oper := br_none;
			v.fence := '0';
			v.fence_i := '0';	
			v.csr_cmd.oper := csr_nop;
		end procedure bubble;
	begin										
		ready := IF_ID.ready32;  -- TODO
		v.ready := ready;
		dc := decoder32_out;
		v.pc := IF_ID.pc;	
		v.insn_len := IF_ID.insn_len;
		v.alu_cmd.oper := dc.alu_oper;
		v.except_cmd := except_cmd_empty;
		v.csr_cmd := dc.csr_cmd;
		v.fence := dc.fence;
		v.fence_i := dc.fence_i; 
		v.atomic_oper := dc.atomic_oper;
		v.amoalu_oper := dc.amoalu_oper;
		
		rs1 := IF_ID.rs1_address;
		rs2 := IF_ID.rs2_address;		
		
		v.alu_cmd.src_rs1 := gpr_out.s1;
		if dc.rs2_imm = '1' then
			v.alu_cmd.src_rs2 := dc.immediate;
		else
		  v.alu_cmd.src_rs2 := gpr_out.s2;
		end if;			

		-- Bypass "s1" operand	 
		if ID_EX.reg_wr = '1' and ID_EX.dest_reg = rs1 and MEM_AL.special = '0' then
				v.alu_cmd.src_rs1 := ex_result;
		elsif EX_MEM.reg_wr = '1' and EX_MEM.dest_reg = rs1 then	
			v.alu_cmd.src_rs1 := EX_MEM.alu_result;
		elsif MEM_AL.reg_wr = '1' and MEM_AL.dest_reg = rs1 then
			v.alu_cmd.src_rs1 := next_AL_WB.reg_wr_data;				
		end if;	
		
		-- Bypass "s2" operand
		if dc.rs2_rd = '1' then
			if ID_EX.reg_wr = '1' and ID_EX.dest_reg = rs2 and MEM_AL.special = '0'  then
				v.alu_cmd.src_rs2 := ex_result;							
			elsif EX_MEM.reg_wr = '1' and EX_MEM.dest_reg = rs2 then	
				v.alu_cmd.src_rs2 := EX_MEM.alu_result;	
			elsif MEM_AL.reg_wr = '1' and MEM_AL.dest_reg = rs2 then
				v.alu_cmd.src_rs2 := next_AL_WB.reg_wr_data;				
			end if;	
		end if;

		-- Stall on register 1-31 write
		operand_stall := '0';
		if ID_EX.reg_wr = '1' and ex_pending = '1' then
			if (dc.rs1_rd = '1' and rs1 /= 0 and ID_EX.dest_reg = rs1) or
				 (dc.rs2_rd = '1' and rs2 /= 0 and ID_EX.dest_reg = rs2) then
				operand_stall := '1';
			end if;	
		end if;	
		if EX_MEM.reg_wr = '1' and EX_MEM.pending = '1' then
			if (dc.rs1_rd = '1' and EX_MEM.dest_reg = rs1) or
				 (dc.rs2_rd = '1' and EX_MEM.dest_reg = rs2) then
				operand_stall := '1';
			end if;			
		end if;	
		
		v.branch_oper := dc.branch_oper;
		v.offset := dc.branch_offset;
		v.immediate := dc.immediate;
		v.mem_sign_ext := dc.mem_sign_ext;
		v.mem_size := dc.mem_size;
		v.mem_rd := dc.mem_rd;
		v.mem_wr := dc.mem_wr;		
		v.reg_wr := dc.reg_wr;
		v.dest_reg := dc.dest_reg;
		-- prevent write to ZERO register
		if dc.dest_reg = ZERO_REG then
		  v.reg_wr := '0';
		end if;	
		
		v.except_cmd.interrupt := '0';
		if ENABLE_HW_BREAKPOINT = 1 and hw_bp_out.raise_exception_exec = '1' and CSR_regs.step_state = step_none then
			v.except_cmd.raised := '1';
			v.except_cmd.cause := EXCEPTION_Breakpoint;			
		elsif ENABLE_HW_BREAKPOINT = 1 and hw_bp_out.enter_debug_mode_exec = '1' and CSR_regs.step_state = step_none  then	
			v.except_cmd.raised := '1';
			v.except_cmd.interrupt := '1';
			v.except_cmd.cause := INT_DebugTrigger;			
		elsif ENABLE_MMU = 1 and (IF_ID.page_fault0 = '1' or IF_ID.page_fault1 = '1') then
			v.except_cmd.raised := '1';
			v.except_cmd.cause := EXCEPTION_InstrFault;		
		elsif ready = '1' and (dc.valid = '0' or IF_ID.invalid = '1') then
			v.except_cmd.raised := '1';
			v.except_cmd.cause := EXCEPTION_InstrIllegal;
		end if;		
		
		if operand_stall = '1' or ready = '0' or flush.ID = '1' then
			bubble;
			v.except_cmd.raised := '0';
		elsif v.except_cmd.raised = '1' then
			bubble;
		end if;		 
		next_ID_EX <= v;
		stall.ID <= (stall.EX or operand_stall) and not flush.ID;
	end process ID_stage;		
	
	EX_stage: process(ID_EX, EX_MEM, MEM_AL, next_PF, alu_out, div_out, CSR_regs, csr_rd_data, stall, flush, jump_cmd)
	  variable v: EX_MEM_stage_type; 
		variable dc: std_logic_vector(WORD_SIZE-1 downto 0);
		variable mul_rs2: std_logic_vector(WORD_SIZE-1 downto 0);
		variable offset: std_logic_vector(WORD_SIZE-1 downto 0);
		variable next_pc: std_logic_vector(WORD_SIZE-1 downto 0);
		variable csr_wr_en: std_logic;
		variable ex_stall: std_logic;
		variable csr_priv: std_logic_vector(1 downto 0);
		
		procedure require_priviledge(priv: std_logic_vector(1 downto 0)) is
		begin										
		  if (ENABLE_S = 1 or ENABLE_U = 1) and CSR_regs.mode < priv then
			  v.except_cmd.raised := '1';
				v.except_cmd.interrupt := '0';
				v.except_cmd.cause := EXCEPTION_InstrIllegal;				
			end if;	
		end procedure require_priviledge;	
		
		procedure bubble is
		begin
			v.ready := '0';
		  v.mem_rd := '0';
		  v.mem_wr := '0';		
		  v.reg_wr := '0';
			v.pending := '0';
			v.special := '0';
			v.branch_oper := br_none;
			v.branch_taken := '0';
		end procedure bubble;
	begin
		v.ready := ID_EX.ready;
		v.pc := ID_EX.pc;
		v.branch_oper := ID_EX.branch_oper;
		v.branch_taken := jump_cmd.branch;
		v.branch_target := next_PF.pc;
		v.reg_wr := ID_EX.reg_wr;
		v.dest_reg := ID_EX.dest_reg;		
		v.except_cmd := ID_EX.except_cmd;
		v.atomic_oper := ID_EX.atomic_oper;
		v.amoalu_oper := ID_EX.amoalu_oper;
		v.pending := '0';
		ex_stall := not div_out.ready;
		dcache_write_back <= '0';
		
	  if ENABLE_C = 1 then
			next_pc := ID_EX.pc + (ID_EX.insn_len & "0");
		else	
		  next_pc := ID_EX.pc + 4;
		end if;		
		case ID_EX.alu_cmd.oper is
			when alu_div | alu_divu | alu_rem | alu_remu =>
			  v.alu_result := div_out.result;
			when others =>
				v.alu_result := alu_out.result;	
		end case;
		ex_result <= v.alu_result; 
		-- return address excluded from forwarding
		if ID_EX.branch_oper = br_jal or ID_EX.branch_oper = br_jalr then
			v.alu_result := next_pc;
		end if;
		
		-- Shift & Multiplication operations
	  dc := decode(ID_EX.alu_cmd.src_rs2(4 downto 0));
		mul_in.rs1 <= ID_EX.alu_cmd.src_rs1;
		v.mul_lo := '0';
		v.mul_hi := '0';
		case ID_EX.alu_cmd.oper is
			when alu_sra | alu_srl =>	
			  if ENABLE_SHIFTER = 0 then
				  if ID_EX.alu_cmd.src_rs2(4 downto 0) /= 0 then
					  for i in 1 to 31 loop
							mul_in.rs2(i) <= dc(32 - i);
						end loop;
						mul_in.rs2(0) <= '0';
						if ID_EX.alu_cmd.oper = alu_sra then
							mul_in.sign1 <= '1';
							mul_in.sign2 <= '0';
						else	
							mul_in.sign1 <= '0';
							mul_in.sign2 <= '0';
						end if;	
					  v.mul_hi := '1'; 
					else
						mul_in.rs2 <= dc;
						mul_in.sign1 <= '0';
						mul_in.sign2 <= '0';					
						v.mul_lo := '1';					
					end if;	
					v.pending := '1';
				end if;
			when alu_sll => 
			  if ENABLE_SHIFTER = 0 then
				  mul_in.rs2 <= dc; 
					mul_in.sign1 <= '0';
					mul_in.sign2 <= '0';
					v.mul_lo := '1';
					v.pending := '1';
				end if;
			when alu_mul =>
        mul_in.rs2 <= ID_EX.alu_cmd.src_rs2;
				mul_in.sign1 <= '1';
				mul_in.sign2 <= '1';
				v.mul_lo := '1';
				v.pending := '1';
			when alu_mulh =>
        mul_in.rs2 <= ID_EX.alu_cmd.src_rs2;
				mul_in.sign1 <= '1';
				mul_in.sign2 <= '1';
				v.mul_hi := '1';
				v.pending := '1';			  
			when alu_mulhu =>
        mul_in.rs2 <= ID_EX.alu_cmd.src_rs2;
				mul_in.sign1 <= '0';
				mul_in.sign2 <= '0';
				v.mul_hi := '1';
				v.pending := '1';			  
			when alu_mulhsu =>
        mul_in.rs2 <= ID_EX.alu_cmd.src_rs2;
				mul_in.sign1 <= '1';
				mul_in.sign2 <= '0';
				v.mul_hi := '1';
				v.pending := '1';	
			when others =>   
			  mul_in.rs2 <= ID_EX.alu_cmd.src_rs2;
				mul_in.sign1 <= '0';
				mul_in.sign2 <= '0';
		end case;	

		if ID_EX.mem_rd = '1' and ID_EX.reg_wr = '1' then
			v.pending := '1';
		end if;	
		ex_pending <= v.pending;
		
		v.mem_sign_ext := ID_EX.mem_sign_ext;
		v.mem_size := ID_EX.mem_size;
		v.mem_rd := ID_EX.mem_rd;
		v.mem_wr := ID_EX.mem_wr;
		offset :=  ID_EX.immediate; -- x"0000" & ID_EX.offset(15 downto 0);
		v.mem_address := ID_EX.alu_cmd.src_rs1 + offset;
		case ID_EX.mem_size is
			when size8 =>
			  v.mem_wr_data := ID_EX.alu_cmd.src_rs2(7 downto 0) & ID_EX.alu_cmd.src_rs2(7 downto 0) &
				  ID_EX.alu_cmd.src_rs2(7 downto 0) & ID_EX.alu_cmd.src_rs2(7 downto 0);
				case v.mem_address(1 downto 0) is
					when "00" =>  v.mem_byteenable := "0001";
					when "01" =>  v.mem_byteenable := "0010";
					when "10" =>  v.mem_byteenable := "0100";
					when "11" =>  v.mem_byteenable := "1000";
					when others => null;
				end case;	
			when size16 =>
			  if (ID_EX.mem_rd = '1') and v.mem_address(0) /= '0' then
					v.except_cmd.cause := EXCEPTION_LoadMisalign;
					v.except_cmd.raised := '1';
				end if;	
			  if (ID_EX.mem_wr = '1') and v.mem_address(0) /= '0' then
					v.except_cmd.cause := EXCEPTION_StoreMisalign;
					v.except_cmd.raised := '1';
				end if;				
			  v.mem_wr_data := ID_EX.alu_cmd.src_rs2(15 downto 0) & ID_EX.alu_cmd.src_rs2(15 downto 0);
				if v.mem_address(1) = '1' then
					v.mem_byteenable := "1100";
				else	
					v.mem_byteenable := "0011";
				end if;	
			when size32 =>	
			  if (ID_EX.mem_rd = '1') and v.mem_address(1 downto 0) /= "00" then
					v.except_cmd.cause := EXCEPTION_LoadMisalign;
					v.except_cmd.raised := '1';
				end if;	
			  if (ID_EX.mem_wr = '1') and v.mem_address(1 downto 0) /= "00" then
					v.except_cmd.cause := EXCEPTION_StoreMisalign;
					v.except_cmd.raised := '1';
				end if;				
			  v.mem_wr_data := ID_EX.alu_cmd.src_rs2;		
				v.mem_byteenable := "1111";
		end case;	
		
		-- CSR operations
		csr_priv := ID_EX.csr_cmd.addr(9 downto 8);
		csr_in.rd_addr <= ID_EX.csr_cmd.addr;
		csr_in.wr_addr <= ID_EX.csr_cmd.addr;
		csr_in.wr_data <= (others => '0');
		csr_in.wr <= '0';	
		if ID_EX.csr_cmd.oper/= csr_nop then
			csr_in.rd <= MEM_AL.special;
		else	
			csr_in.rd <= '0';
		end if;	
		csr_in.eret <= '0';
		csr_in.dret <= '0';
		csr_in.priv <= csr_priv;
		csr_in.flush_tlb <= '0';
		csr_wr_en := MEM_AL.special;
		case ID_EX.csr_cmd.oper is
	    when csr_nop =>  null;
		  when csr_rw =>
			  csr_in.wr_data <= ID_EX.alu_cmd.src_rs1;
				csr_in.wr <= csr_wr_en and ID_EX.csr_cmd.write;
		  when csr_rs =>
			  csr_in.wr_data <= csr_rd_data or ID_EX.alu_cmd.src_rs1;
				csr_in.wr <= csr_wr_en and ID_EX.csr_cmd.write;
		  when csr_rc =>
			  csr_in.wr_data <= csr_rd_data and (not ID_EX.alu_cmd.src_rs1);
				csr_in.wr <= csr_wr_en and ID_EX.csr_cmd.write;
		  when csr_rwi =>
			  csr_in.wr_data(WORD_SIZE-1 downto 5) <= (others => '0');
				csr_in.wr_data(4 downto 0) <= ID_EX.csr_cmd.imm;
				csr_in.wr <= csr_wr_en and ID_EX.csr_cmd.write;
		  when csr_rsi =>
			  csr_in.wr_data(WORD_SIZE-1 downto 5) <= csr_rd_data(WORD_SIZE-1 downto 5);
				csr_in.wr_data(4 downto 0) <= csr_rd_data(4 downto 0) or ID_EX.csr_cmd.imm;	
				csr_in.wr <= csr_wr_en and ID_EX.csr_cmd.write;
		  when csr_rci =>
			  csr_in.wr_data(WORD_SIZE-1 downto 5) <= csr_rd_data(WORD_SIZE-1 downto 5);
				csr_in.wr_data(4 downto 0) <= csr_rd_data(4 downto 0) and (not ID_EX.csr_cmd.imm);	
				csr_in.wr <= csr_wr_en and ID_EX.csr_cmd.write;
			when csr_ret =>													
			  if ID_EX.csr_cmd.addr(10) = '1' then 
				  require_priviledge(csr_priv);
					csr_in.dret <= csr_wr_en;
				else	
					require_priviledge(PRV_M);
				  csr_in.eret <= csr_wr_en;
				end if;	
			when csr_wfi =>
			  if CSR_get_ip(CSR_regs.MIP) = 0 then
					ex_stall := '1';
				end if;
			when csr_call =>
			  v.except_cmd.raised := '1';
				v.except_cmd.interrupt := '1';
				case CSR_regs.mode is
					when PRV_M =>  v.except_cmd.cause := INT_MachineSoftware;
					when PRV_S =>	
					  if ENABLE_S = 1 then
							v.except_cmd.cause := INT_SupervisorSoftware;
						end if;	
					when PRV_U =>	
					  if ENABLE_U = 1 then
							v.except_cmd.cause := INT_UserSoftware;
						end if;					
					when PRV_H =>
--					  if ENABLE_H = 1 then
--							v.except_cmd.cause := INT_HypervisorSoftware;
--						end if;					
          when others => null;
				end case;	
			when csr_break	=>
			  v.except_cmd.raised := '1';
				v.except_cmd.interrupt := '0';
				v.except_cmd.cause := EXCEPTION_Breakpoint;
			when csr_sfence_vm =>
			  if ENABLE_S = 1 and ENABLE_MMU = 1 then
				  require_priviledge(PRV_S);
				  csr_in.flush_tlb <= MEM_AL.special;
					dcache_write_back <= MEM_AL.special;
				end if;	
		end case;
		-- Check CSR access rights
    if ID_EX.csr_cmd.oper = csr_rw or ID_EX.csr_cmd.oper = csr_rwi or 
			 ID_EX.csr_cmd.oper = csr_rs or ID_EX.csr_cmd.oper = csr_rsi or 
			 ID_EX.csr_cmd.oper = csr_rc or ID_EX.csr_cmd.oper = csr_rci then
			if validate_csr(ID_EX.csr_cmd.addr, CSR_regs.mode, ID_EX.csr_cmd.write) = '0' then
			  v.except_cmd.raised := '1';
				v.except_cmd.interrupt := '0';
				v.except_cmd.cause := EXCEPTION_InstrIllegal;				
			end if;	
		end if;	
		
		if ENABLE_DEBUG = 1 and ID_EX.ready = '1' and CSR_regs.step_state = step_stepping then
			ex_stall := '1';
		end if;	
		
		if flush.EX = '1' or ex_stall = '1' then
			bubble;
			v.except_cmd.raised := '0';
		elsif v.except_cmd.raised = '1' then
			bubble;
    end if;	
		if flush.EX = '0' and (ID_EX.csr_cmd.oper /= csr_nop or ID_EX.fence = '1') then
			v.special := not (EX_MEM.special or MEM_AL.special);
			v.reg_wr := v.reg_wr and v.special;
			if MEM_AL.special = '0' then
				ex_stall := '1';
			end if;	
		else	
			v.special := '0';
		end if;	

		next_EX_MEM <= v;
		stall.EX <= (stall.MEM or ex_stall) and not flush.EX;
	end process EX_stage;	

	MEM_stage: process(EX_MEM, dcache_out, mul_out, stall, flush)
	  variable v: MEM_AL_stage_type; 
		variable muxa: std_logic_vector(1 downto 0);
		variable page_fault: std_logic;
		
		procedure bubble is
		begin	
			v.ready := '0';
			v.reg_wr := '0'; 
			v.mem_rd := '0';
			v.mem_wr := '0';
			v.special := '0';	
			v.branch_oper := br_none;
			v.branch_taken := '0';
			v.load_reservation := MEM_AL.load_reservation;
		end procedure bubble;	
	begin
		v.ready := EX_MEM.ready;
		if EX_MEM.ready = '1' then  -- for correct jump tracing
		  v.pc := EX_MEM.pc; 
		end if;	
		page_fault := '0'; -- TODO
		v.special := EX_MEM.special;
		v.mem_rd := EX_MEM.mem_rd;
		v.mem_wr := EX_MEM.mem_wr;
		v.mem_wr_data := EX_MEM.mem_wr_data;
		v.branch_oper := EX_MEM.branch_oper;
		v.branch_taken := EX_MEM.branch_taken;		
		v.branch_target := EX_MEM.branch_target;
		v.load_reservation := MEM_AL.load_reservation;
		v.sc_cond := '0';
		
		muxa := EX_MEM.mul_hi & EX_MEM.mul_lo;
		case muxa is
			when "10" =>   v.reg_wr_data := mul_out.result(63 downto 32);
			when "01" =>   v.reg_wr_data := mul_out.result(31 downto 0);
			when others => v.reg_wr_data := EX_MEM.alu_result;
		end case;	
		
		v.mem_sign_ext := EX_MEM.mem_sign_ext;
		v.mem_size := EX_MEM.mem_size;
		v.mem_rd := EX_MEM.mem_rd;
		v.mem_address := EX_MEM.mem_address;
		v.mem_rd_data := dcache_out.data;
		v.reg_wr := EX_MEM.reg_wr;
		v.dest_reg := EX_MEM.dest_reg;
		
		v.except_cmd := EX_MEM.except_cmd;
		if ENABLE_HW_BREAKPOINT = 1 and hw_bp_out.raise_exception_mem = '1' and CSR_regs.step_state = step_none  then
			v.except_cmd.raised := '1';	
			v.except_cmd.interrupt := '0';
			v.except_cmd.cause := EXCEPTION_Breakpoint;			
		elsif ENABLE_HW_BREAKPOINT = 1 and hw_bp_out.enter_debug_mode_mem = '1' and CSR_regs.step_state = step_none then	
			v.except_cmd.raised := '1';
			v.except_cmd.interrupt := '0';
			v.except_cmd.cause := INT_DebugTrigger;			
		elsif ENABLE_MMU = 1 then
			if page_fault = '1' then
				v.except_cmd.raised := '1';
				v.except_cmd.interrupt := '0';
				if EX_MEM.mem_rd = '1' then
					v.except_cmd.cause := EXCEPTION_LoadFault;
				end if;
				if EX_MEM.mem_wr = '1' then
					v.except_cmd.cause := EXCEPTION_StoreFault;
				end if;	
			end if;	
		end if;		

		if ENABLE_A = 1 then
		  case EX_MEM.atomic_oper is
				when at_lr => 
				  v.load_reservation := EX_MEM.mem_address;
				when at_sc =>	
				  if EX_MEM.mem_address = MEM_AL.load_reservation then
						v.sc_cond := '1';
					else	
						v.sc_cond := '0';
						v.mem_wr := '0';
					end if;
				when others => null;
			end case;	
		end if;		
		
		if dcache_out.busy = '1' or flush.MEM = '1' then
			bubble;
			v.except_cmd.raised := '0';
		elsif v.except_cmd.raised = '1' then
			bubble;
		end if;
		next_MEM_AL <= v;
		stall.MEM <= (stall.AL or dcache_out.busy) and not flush.MEM;
	end process MEM_stage;	

	AL_stage: process(MEM_AL, CSR_regs, csr_rd_data, stall, flush, trace_out)
	  variable v: AL_WB_stage_type;
		
		procedure bubble is
		begin
			v.ready := '0';
			v.reg_wr := '0';
		end procedure bubble;		
	begin
		v.ready := MEM_AL.ready;
		v.pc := MEM_AL.pc;
		
		if MEM_AL.mem_rd = '1' then
			case MEM_AL.mem_size is
				when size8 =>
				  case MEM_AL.mem_address(1 downto 0) is
						when "00" =>  v.reg_wr_data(7 downto 0) := MEM_AL.mem_rd_data(7 downto 0);
						when "01" =>	v.reg_wr_data(7 downto 0) := MEM_AL.mem_rd_data(15 downto 8);
						when "10" =>	v.reg_wr_data(7 downto 0) := MEM_AL.mem_rd_data(23 downto 16);
						when "11" =>	v.reg_wr_data(7 downto 0) := MEM_AL.mem_rd_data(31 downto 24);
						when others => null;
					end case;	 
					if MEM_AL.mem_sign_ext = '1' and v.reg_wr_data(7) = '1'	then
						v.reg_wr_data(31 downto 8) := x"FFFFFF";
					else	
						v.reg_wr_data(31 downto 8) := x"000000";
					end if;					
				when size16 =>
				  if MEM_AL.mem_address(1) = '1' then
						v.reg_wr_data(15 downto 0) := MEM_AL.mem_rd_data(31 downto 16);
					else	
						v.reg_wr_data(15 downto 0) := MEM_AL.mem_rd_data(15 downto 0);
					end if;
					if MEM_AL.mem_sign_ext = '1' and v.reg_wr_data(15) = '1' then
						v.reg_wr_data(31 downto 16) := x"FFFF";
					else	
						v.reg_wr_data(31 downto 16) := x"0000";
					end if;
				when size32 =>
					v.reg_wr_data := MEM_AL.mem_rd_data;
			end case;	
		elsif MEM_AL.special = '1' then
			v.reg_wr_data := csr_rd_data_reg;
		else	
		  v.reg_wr_data := MEM_AL.reg_wr_data;
		end if;	

		if ENABLE_A = 1 then
		  if MEM_AL.atomic_oper = at_sc then
				v.reg_wr_data(WORD_SIZE-1 downto 1) := (others => '0');
				v.reg_wr_data(0) := MEM_AL.sc_cond;
			end if;	
		end if;		
		
		v.reg_wr := MEM_AL.reg_wr;
		v.dest_reg := MEM_AL.dest_reg;
		
		if flush.AL = '1' then
			bubble;
		end if;	
		
		next_AL_WB <= v;
		stall.AL <= trace_out.stall; 
	end process AL_stage;	
	
	stage_state: process(reset, clk)
	begin
		if reset = '1' then
			PF.pc <= START_ADDRESS;
			PF.jump <= '0';
			PF.ready <= '0';
			DC <= DC_empty;
			IF_ID <= IF_ID_empty;
			ID_EX <= ID_EX_empty;
			EX_MEM <= EX_MEM_empty;
			MEM_AL <= MEM_AL_empty;
			AL_WB <= AL_WB_empty;
		elsif rising_edge(clk) then	
		  PF <= next_PF;
			if ENABLE_C = 1 and stall.DC = '0' then
				DC <= next_DC;
			end if;				
			if stall.ID = '0' then
		    IF_ID <= next_IF_ID;
			end if;	
			if stall.EX = '0' then
			  ID_EX <= next_ID_EX;
			end if;
			if stall.MEM = '0' then
			  EX_MEM <= next_EX_MEM;
			end if;
			if stall.AL = '0' then
			  MEM_AL <= next_MEM_AL;
			end if;	
		  AL_WB <= next_AL_WB;
		end if;
	end process stage_state;
	
	csr_proc: process(CSR_regs, csr_in, MEM_AL, next_AL_WB, mtip, msip, meip, dbgm_bus_in)
	  variable v: CSR_regs_type;
		variable epc: std_logic_vector(WORD_SIZE-1 downto 0);
		variable ejmp: std_logic;
    variable irq_raised: std_logic; 
		variable irq_cause: cause_type;
		variable intr: std_logic;
		
    procedure set_mode(mode: in std_logic_vector(1 downto 0)) is
		begin
      if mode = PRV_H then
        v.mode := PRV_U;	
			else
				v.mode := mode;
			end if;	
			tlb_flush <= '1';
    end procedure set_mode;
		
		procedure set_ie(priv: in std_logic_vector(1 downto 0); ie: in std_logic) is
		begin
			case priv is
				when PRV_U =>
				  if ENABLE_U = 1 then
						v.mstatus.UIE := ie;
					end if;
				when PRV_S =>
				  if ENABLE_S = 1 then
						v.mstatus.SIE := ie;
					end if;	
				when PRV_H => 
--				  if ENABLE_S = 1 then
--						v.mstatus.HIE := ie;
--					end if;				
				when PRV_M =>
				  v.mstatus.MIE := ie;
				when others => null;
			end case;	
		end procedure set_ie;		
		
		procedure ejump(address: in std_logic_vector(WORD_SIZE-1 downto 0)) is
		begin
			epc := address;		
			ejmp := '1';
		end procedure ejump;	
		
	  procedure enter_debug_mode(cause: in std_logic_vector(2 downto 0)) is
	  begin
     v.dcsr.cause := cause;
     v.dcsr.prv := CSR_regs.mode;
		 v.dcsr.debug := '1';
     set_mode(PRV_M);
     v.dpc := MEM_AL.pc;
     ejump(DEBUG_ROM_START);
    end procedure enter_debug_mode;
	 
	  procedure do_trap(cause: in cause_type; intr: in std_logic) is
		  variable deleg: std_logic_vector(WORD_SIZE-1 downto 0);
			variable ens: std_logic;
	  begin
		  if ENABLE_DEBUG = 1 then
        if intr = '0' and cause = EXCEPTION_Breakpoint and 
				  (
            (CSR_regs.mode = PRV_M and CSR_regs.dcsr.ebreakm = '1') or
            (CSR_regs.mode = PRV_H and CSR_regs.dcsr.ebreakh = '1') or
            (CSR_regs.mode = PRV_S and CSR_regs.dcsr.ebreaks = '1') or
            (CSR_regs.mode = PRV_U and CSR_regs.dcsr.ebreaku = '1')) then
         enter_debug_mode(DCSR_CAUSE_SWBP);
			  end if;	 

        if CSR_regs.dcsr.cause = 1 then
          ejump(DEBUG_ROM_EXCEPTION);
			  end if; 
		  end if; 
		 
			if ENABLE_S = 1 then
		    if intr = '1' then
			    deleg := CSR_regs.mideleg;
		    else	
			    deleg := CSR_regs.medeleg;
		    end if;	
				ens := deleg(CONV_INTEGER(cause));
			else	
				ens := '0';
			end if;
		 	if ENABLE_S = 1 and ens = '1' then
    		ejump(CSR_regs.stvec);
        v.scause(WORD_SIZE-1) := intr;
				v.scause(cause'range) := cause;
        v.sepc := MEM_AL.pc; 
				if ENABLE_BADADDR = 1 then
          v.sbadaddr := MEM_AL.mem_address; 
        end if;
				v.mstatus.SPIE := get_ie(CSR_regs, CSR_regs.mode);
				v.mstatus.SPP := CSR_regs.mode(0);
				v.mstatus.SIE := '0';
        set_mode(PRV_S);				 
			else	 
   		  ejump(CSR_regs.mtvec);
        v.mcause(WORD_SIZE-1) := intr;
				v.mcause(cause'range) := cause;
        v.mepc := MEM_AL.pc; 
				if ENABLE_BADADDR = 1 then
          v.mbadaddr := MEM_AL.mem_address;
        end if;													 
				v.mstatus.MPIE := get_ie(CSR_regs, CSR_regs.mode);
				v.mstatus.MPP := CSR_regs.mode;
				v.mstatus.MIE := '0';				 
			end if;	 
	  end procedure do_trap;
		
		procedure do_interrupts(variable irq_raised: inout std_logic; variable irq_cause: inout cause_type) is
		  variable pending_irqs: std_logic_vector(WORD_SIZE-1 downto 0);
			variable active_irqs: std_logic_vector(WORD_SIZE-1 downto 0);
		begin
			active_irqs := (others => '0');
			pending_irqs := CSR_get_ie(CSR_regs.mie) and CSR_get_ip(CSR_regs.mip);
			if ENABLE_S = 1 then
			  if CSR_regs.mode /= PRV_M or
				    (CSR_regs.mode = PRV_M and CSR_regs.mstatus.MIE = '1') then
					active_irqs := pending_irqs and not CSR_regs.mideleg;
				end if;	
				if CSR_regs.mode < PRV_S or
				    (CSR_regs.mode = PRV_S and CSR_regs.mstatus.SIE = '1') then
					active_irqs := active_irqs or (pending_irqs and CSR_regs.mideleg);
				end if;	
			else 
				if CSR_regs.mstatus.MIE = '1' then
				  active_irqs := pending_irqs;
				end if;	
			end if;
			
			if ENABLE_S = 1 then
				if CSR_regs.mode = PRV_U or
					  (CSR_regs.mode = PRV_S and CSR_regs.mstatus.SIE = '1') then
					 active_irqs := active_irqs or (pending_irqs and CSR_regs.mideleg);
				 end if;
			end if;	
			
			if active_irqs(11 downto 0) /= 0 then
				irq_raised := '1';
			end if;
			-- Interrupt encoder - least bit has higher priority
			for i in 11 downto 0 loop
				if active_irqs(i) = '1' then
					irq_cause := CONV_STD_LOGIC_VECTOR(i, 4);
				end if;	
			end loop;	
		end procedure do_interrupts;		

		procedure do_eret(priv: in std_logic_vector(1 downto 0)) is
		begin
			case priv is					
				when PRV_M => -- MRET		
				  set_ie(priv, CSR_regs.mstatus.MPIE);
				  v.mstatus.MPIE := '0'; 
					if ENABLE_U = 1 then
					  v.mstatus.MPP := PRV_U;
					else
--						v.mstatus.MPP := PRV_M;
					end if;	
				  ejump(CSR_regs.mepc);
					set_mode(CSR_regs.mstatus.MPP);
				when PRV_S =>  -- SRET
				  set_ie(priv, CSR_regs.mstatus.SPIE);
				  v.mstatus.SPIE := '0';				
					v.mstatus.SPP := PRV_U(0);
				  ejump(CSR_regs.sepc);
					set_mode(CSR_regs.mstatus.MPP);				
				when PRV_U =>  null;
				when PRV_H =>  null;
				when others => null;				
			end case;
		end procedure do_eret;	
		
		procedure do_dret is
		begin	
      ejump(CSR_regs.dpc);
      set_mode(CSR_regs.dcsr.prv);
      v.dcsr.cause := (others => '0');
			v.dcsr.debug := '0';
      if CSR_regs.dcsr.step = '1' then
        v.step_state := step_stepping;
			end if;	
		end procedure do_dret;	
		
		function CSR_ccsr_read(ccsr: in CSR_ccsr_type) return std_logic_vector is
		  variable rd_data: std_logic_vector(WORD_SIZE-1 downto 0);
		begin
			rd_data := (others => '0');
			rd_data(31) := ccsr.authenificated;
			rd_data(30) := ccsr.authbusy;
			rd_data(25) := ccsr.stopcycle;
			rd_data(24) := ccsr.stoptime;
			rd_data(23) := ccsr.frozen;
			rd_data(22) := ccsr.freezesup;
			rd_data(21) := ccsr.freeze;
			rd_data(19) := ccsr.halted;
			rd_data(18) := ccsr.haltsup;
			rd_data(17) := ccsr.halt;
			rd_data(7 downto 0) := ccsr.interrupt;
			return rd_data;
		end function CSR_ccsr_read;	
		
		procedure CSR_ccsr_write(variable ccsr: inout CSR_ccsr_type; 
		    wr_data: in std_logic_vector(WORD_SIZE-1 downto 0)) is
		begin
		  ccsr.ndreset := wr_data(27);
			ccsr.fullreset := wr_data(26);
			ccsr.stopcycle := wr_data(25);
			ccsr.stoptime := wr_data(24);
			ccsr.freeze := wr_data(21);
			ccsr.halt := wr_data(17);
			ccsr.resume := wr_data(16);
			ccsr.interrupt := wr_data(7 downto 0);
		end procedure CSR_ccsr_write;
		
		function CSR_dcsr_read(dcsr: in CSR_dcsr_type) return std_logic_vector is
		  variable rd_data: std_logic_vector(WORD_SIZE-1 downto 0);
		begin
			rd_data := (others => '0');
			rd_data(31) := dcsr.pcsample;
			rd_data(29 downto 28) := dcsr.xdebugver;
			rd_data(27 downto 16) := dcsr.hwbpcount;
			rd_data(15) := dcsr.ebreakm;
			rd_data(14) := dcsr.ebreakh;
			rd_data(13) := dcsr.ebreaks;
			rd_data(12) := dcsr.ebreaku;		
			rd_data(9) := dcsr.debug;
			rd_data(8 downto 6) := dcsr.cause;
			rd_data(5) := dcsr.debugint;
			rd_data(3) := dcsr.haltinterrupt;		
			rd_data(2) := dcsr.step;
			rd_data(1 downto 0) := dcsr.prv;			
			return rd_data;
		end function CSR_dcsr_read;	
		
		procedure CSR_dcsr_write(variable dcsr: inout CSR_dcsr_type; 
		    wr_data: in std_logic_vector(WORD_SIZE-1 downto 0)) is
		begin														
			dcsr.ebreakm := wr_data(15);
--			if ENABLE_H = 1 then
--			  dcsr.ebreakh := wr_data(14);
--			end if;	
			if ENABLE_S = 1 then
			  dcsr.ebreaks := wr_data(13);
			end if;
			if ENABLE_U = 1 then
			  dcsr.ebreaku := wr_data(12);
			end if;											
			dcsr.haltinterrupt := wr_data(3);			
			dcsr.step := wr_data(2); -- flag from version 0.11
		end procedure CSR_dcsr_write;		
		
		function CSR_read(CSR_regs: CSR_regs_type; address: std_logic_vector(11 downto 0); 
		    dbgm_bus_in: dbgm_bus_in_type) return std_logic_vector is
      variable rd_data: std_logic_vector(WORD_SIZE-1 downto 0);		
		begin
		rd_data := (others => '0');
			case address is
	      when CSR_ustatus =>   
	      when CSR_uie =>
	      when CSR_utvec =>
	      when CSR_uscratch => 
	      when CSR_uepc =>  	 
	      when CSR_ucause =>
	      when CSR_ubadaddr =>
	      when CSR_uip =>
	      when CSR_fflags =>
	      when CSR_frm =>
	      when CSR_fcsr =>
	      when CSR_cycle =>	 -- Cycle counter for RDCYCLE instruction
				  if ENABLE_MCYCLE = 1 then
						rd_data := CSR_regs.mcycle(WORD_SIZE-1 downto 0);
					end if;	
	      when CSR_time =>	-- Timer for RDTIME instruction
	      when CSR_instret =>	 -- Instructions-retired counter for RDINSTRET instruction
				  if ENABLE_MINSTRET = 1 then
					  rd_data := CSR_regs.minstret(WORD_SIZE-1 downto 0);
					end if;
	      when CSR_hpmcounter3 =>
	      when CSR_hpmcounter4 =>
	      when CSR_hpmcounter31 =>
	      when CSR_cycleh =>
				  if ENABLE_MCYCLE = 1 and WORD_SIZE = 32 then
						rd_data := CSR_regs.mcycle(63 downto WORD_SIZE);
					end if;			
	      when CSR_timeh =>	 -- Upper 32 bits of time, RV32I only
	      when CSR_instreth =>  -- Upper 32 bits of instret, RV32I only
				  if ENABLE_MINSTRET = 1 and WORD_SIZE = 32 then
					  rd_data := CSR_regs.minstret(63 downto WORD_SIZE);
					end if;			
	      when CSR_hpmcounter3h =>
	      when CSR_hpmcounter4h =>
	      when CSR_hpmcounter31h =>
	      when CSR_sstatus =>	 -- Supervisor status register
				  if ENABLE_S = 1 then
						rd_data := CSR_get_mstatus(CSR_regs.mstatus);
						rd_data(WORD_SIZE-2 downto 19) := (others => '0');
						rd_data(12 downto 9) := (others => '0');
						rd_data(7 downto 6) := (others => '0');
						rd_data(3 downto 2) := (others => '0');
					end if;	
	      when CSR_sedeleg =>
	      when CSR_sideleg =>
	      when CSR_sie =>	 -- Supervisor interrupt-enable register
				  if ENABLE_S = 1 then
						rd_data := CSR_get_ie(CSR_regs.mie) and CSR_regs.mideleg;
					end if;	
	      when CSR_stvec =>	  -- Supervisor Trap-Vector Base-Address Register
				  if ENABLE_S = 1 then
						rd_data := CSR_regs.stvec;
					end if;
	      when CSR_sscratch =>	-- Supervisor scratch register
				  if ENABLE_S = 1 then
						rd_data := CSR_regs.sscratch;
					end if;			
	      when CSR_sepc =>   -- Supervisor exception program counter
				  if ENABLE_S = 1 then
						rd_data := CSR_regs.sepc;
					end if;			
	      when CSR_scause =>  -- Supervisor trap cause
				  if ENABLE_S = 1 then
						rd_data := CSR_regs.scause;
					end if;			
	      when CSR_sbadaddr =>  -- Supervisor bad address
				  if ENABLE_S = 1 and ENABLE_BADADDR = 1 then
						rd_data := CSR_regs.sbadaddr;
					end if;			
	      when CSR_sip =>	 -- Supervisor interrupt pending
				  if ENABLE_S = 1 then
						rd_data := CSR_get_ip(CSR_regs.mip) and CSR_regs.mideleg;
					end if;
	      when CSR_sptbr =>	 -- Page-table base register
				  if ENABLE_S = 1 then
						rd_data := CSR_regs.sptbr.ASID & CSR_regs.sptbr.PPN;
					end if;			
	      when CSR_hstatus =>
	      when CSR_hedeleg =>
	      when CSR_hideleg =>
	      when CSR_hie =>
	      when CSR_htvec =>
	      when CSR_hscratch =>
	      when CSR_hepc =>
	      when CSR_hcause =>
	      when CSR_hbadaddr =>
	      when CSR_hip =>
	      when CSR_TBD =>
	      when CSR_mvendorid =>
	      when CSR_marchid =>
	      when CSR_mimpid =>
	      when CSR_mhartid =>	
				  rd_data := (others => '0');  -- Only one hart implemented
	      when CSR_mstatus =>		-- Machine mode status register
					rd_data := CSR_get_mstatus(CSR_regs.mstatus);
	      when CSR_misa =>	 -- ISA and extensions
				  if ENABLE_MISA = 1 then
					  case WORD_SIZE is
							when 32 => rd_data(WORD_SIZE-1 downto WORD_SIZE-2) := "01";
							when 64 => rd_data(WORD_SIZE-1 downto WORD_SIZE-2) := "10";
							when others => null;
						end case;
						rd_data(0) := to_std_logic(ENABLE_A);
						rd_data(2) := to_std_logic(ENABLE_C);
						rd_data(3) := to_std_logic(ENABLE_D);
						rd_data(4) := to_std_logic(ENABLE_E);
						rd_data(5) := to_std_logic(ENABLE_F);
						rd_data(6) := to_std_logic(ENABLE_G);
						rd_data(8) := '1';  -- I
						rd_data(12) := to_std_logic(ENABLE_M);
						rd_data(13) := to_std_logic(ENABLE_N);
						rd_data(18) := to_std_logic(ENABLE_S);
						rd_data(20) := to_std_logic(ENABLE_U);
						rd_data(23) := to_std_logic(ENABLE_X);
					end if;
	      when CSR_medeleg =>	 -- Machine exception delegation register
				  if ENABLE_S = 1 or ENABLE_U = 1 then
					  rd_data := CSR_regs.medeleg;
					end if;
	      when CSR_mideleg =>	 -- Machine interrupt delegation register
				  if ENABLE_S = 1 or ENABLE_U = 1 then
						rd_data := CSR_regs.mideleg;
					end if;			
	      when CSR_mie =>	 -- Machine interrupt-enable register
				  rd_data(31 downto 0) := CSR_get_ie(CSR_regs.mie);
	      when CSR_mtvec =>		  -- Machine Trap-Vector Base-Address Register
				  rd_data := CSR_regs.mtvec;
	      when CSR_mscratch =>	-- Machine scratch register
				  rd_data := CSR_regs.mscratch;
	      when CSR_mepc =>  -- Machine exception program counter
				  rd_data := CSR_regs.mepc;
	      when CSR_mcause =>	-- Machine trap cause
				  rd_data(3 downto 0) := CSR_regs.mcause(3 downto 0);
					rd_data(WORD_SIZE-1) := CSR_regs.mcause(WORD_SIZE-1);
	      when CSR_mbadaddr => 	-- Machine bad address
				  if ENABLE_BADADDR = 1 then
				    rd_data := CSR_regs.mbadaddr;
					end if;	
	      when CSR_mip =>	 -- Machine interrupt pending
					rd_data := CSR_get_ip(CSR_regs.mip);			  
	      when CSR_mbase =>
	      when CSR_mbound =>
	      when CSR_mibase =>
	      when CSR_mibound =>
	      when CSR_mdbase =>
	      when CSR_mdbound =>
	      when CSR_mcycle =>  -- Machine cycle counter
				  if ENABLE_MCYCLE = 1 then
						rd_data := CSR_regs.mcycle(WORD_SIZE-1 downto 0);
					end if;				
	      when CSR_minstret =>  -- Machine instructions-retired counter
				  if ENABLE_MINSTRET = 1 then
					  rd_data := CSR_regs.minstret(WORD_SIZE-1 downto 0);
					end if;			
	      when CSR_mhpmcounter3 =>
	      when CSR_mhpmcounter4 =>
	      when CSR_mhpmcounter31 =>
	      when CSR_mcycleh =>	 -- Upper 32 bits of mcycle, RV32I only
				  if ENABLE_MCYCLE = 1 and WORD_SIZE = 32 then
						rd_data := CSR_regs.mcycle(63 downto WORD_SIZE);
					end if;			
	      when CSR_minstreth =>	 -- Upper 32 bits of minstret, RV32I only
				  if ENABLE_MINSTRET = 1 and WORD_SIZE = 32 then
					  rd_data := CSR_regs.minstret(63 downto WORD_SIZE);
					end if;			
	      when CSR_mhpmcounter3h =>
	      when CSR_mhpmcounter4h =>
	      when CSR_mhpmcounter31h =>
	      when CSR_mucounteren =>
	      when CSR_mscounteren =>
	      when CSR_mhcounteren =>
	      when CSR_mhpmevent3 =>
	      when CSR_mhpmevent4 =>
	      when CSR_mhpmevent31 =>
	      when CSR_tselect =>
	      when CSR_tdata1 =>
	      when CSR_tdata2 =>
	      when CSR_tdata3 =>
	      when CSR_dcsr =>	
				  if ENABLE_DEBUG = 1 then
					  rd_data := CSR_dcsr_read(CSR_regs.dcsr);
					end if;
	      when CSR_dpc =>
				  if ENABLE_DEBUG = 1 then
						rd_data := CSR_regs.dpc;
					end if;	
	      when CSR_dscratch	=> 
				  if ENABLE_DEBUG_ROM1 = 1 then
						rd_data := CSR_regs.dscratch;
					end if;				
				when CSR_busstate	=>
				  if ENABLE_DEBUG_ROM2 = 1 then
					  rd_data(1) := dbgm_bus_in.chipselect and dbgm_bus_in.write;
						rd_data(0) := dbgm_bus_in.chipselect and dbgm_bus_in.read;
					end if;				  
        when CSR_busaddress =>
				  if ENABLE_DEBUG_ROM2 = 1 then
						rd_data(dbgm_bus_in.address'range) := dbgm_bus_in.address;
					end if;				
        when CSR_busdata =>
				  if ENABLE_DEBUG_ROM2 = 1 then
						rd_data := dbgm_bus_in.writedata;					
					end if;	
	      when CSR_dpc0 =>
				  if ENABLE_DEBUG_ROM2 = 1 then
						rd_data := CSR_regs.dpc;
					end if;					
        when CSR_dscratch0 =>
				  if ENABLE_DEBUG_ROM2 = 1 then
						rd_data := CSR_regs.dscratch0;
					end if;				
        when CSR_dscratch1 =>
				  if ENABLE_DEBUG_ROM2 = 1 then
					  rd_data := CSR_regs.dscratch1;
					end if;				
        when CSR_ccsr =>
				  if ENABLE_DEBUG = 1 then
						rd_data := CSR_ccsr_read(CSR_regs.ccsr);
					end if;				
        when CSR_cdtmaddress =>	
				  if ENABLE_DEBUG = 1 then
						rd_data(CSR_regs.cdtmaddress.dtmaddress'range) := CSR_regs.cdtmaddress.dtmaddress;
						rd_data(1) := CSR_regs.cdtmaddress.wired;
						rd_data(0) := CSR_regs.cdtmaddress.cdisable;
					end if;				
				when others =>  null;			
			end case;
			return rd_data;
		end function CSR_read;	
		
		procedure CSR_write(variable v: inout CSR_regs_type; address: in std_logic_vector(11 downto 0); 
		    wr_data: in std_logic_vector(WORD_SIZE-1 downto 0)) is
			variable status: CSR_mstatus_type;
		begin
			case address is
	      when CSR_ustatus =>   
	      when CSR_uie =>
	      when CSR_utvec =>
	      when CSR_uscratch => 
	      when CSR_uepc =>  	 
	      when CSR_ucause =>
	      when CSR_ubadaddr =>
	      when CSR_uip =>
	      when CSR_fflags =>
	      when CSR_frm =>
	      when CSR_fcsr =>
	      when CSR_cycle =>	 -- Cycle counter for RDCYCLE instruction
			    if ENABLE_MCYCLE = 1 then
					  v.mcycle(WORD_SIZE-1 downto 0) := wr_data;
				  end if;				
	      when CSR_time =>	-- Timer for RDTIME instruction
	      when CSR_instret =>	 -- Instructions-retired counter for RDINSTRET instruction
			    if ENABLE_MINSTRET = 1 then
					  v.minstret(WORD_SIZE-1 downto 0) := wr_data;
				  end if;				
	      when CSR_hpmcounter3 =>
	      when CSR_hpmcounter4 =>
	      when CSR_hpmcounter31 =>
	      when CSR_cycleh => 	-- Upper 32 bits of cycle, RV32I only
			    if ENABLE_MCYCLE = 1 and WORD_SIZE = 32 then
					  v.mcycle(63 downto WORD_SIZE) := wr_data;
				  end if;				
	      when CSR_timeh =>	 -- Upper 32 bits of time, RV32I only
	      when CSR_instreth =>  -- Upper 32 bits of instret, RV32I only
			    if ENABLE_MINSTRET = 1 and WORD_SIZE = 32 then
					  v.minstret(63 downto WORD_SIZE) := wr_data;
				  end if;				
	      when CSR_hpmcounter3h =>
	      when CSR_hpmcounter4h =>
	      when CSR_hpmcounter31h =>
	      when CSR_sstatus =>	 -- Supervisor status register
				  if ENABLE_S = 1 then
					  CSR_set_mstatus(status, wr_data);
					  v.mstatus.SIE := status.SIE;
            v.mstatus.SPIE := status.SPIE;
            v.mstatus.SPP := status.SPP;
            v.mstatus.PUM := status.PUM;
            v.mstatus.FS := status.FS;
					end if;
	      when CSR_sedeleg =>
	      when CSR_sideleg =>
	      when CSR_sie =>	 -- Supervisor interrupt-enable register
				  CSR_set_ie(v.mie, (wr_data and CSR_regs.mideleg) or 
				                    (CSR_get_ie(CSR_regs.mie) and not CSR_regs.mideleg));
	      when CSR_stvec =>	 -- Supervisor trap handler base address
				  if ENABLE_S = 1 then
						 v.stvec(WORD_SIZE-1 downto 2) := wr_data(WORD_SIZE-1 downto 2);
					end if;	
	      when CSR_sscratch =>	-- Scratch register for supervisor trap handlers
				  if ENABLE_S = 1 then
						v.sscratch := wr_data;
					end if;				
	      when CSR_sepc =>	-- Supervisor exception program counter
				  if ENABLE_S = 1 then
				    v.sepc(WORD_SIZE-1 downto 2) := wr_data(WORD_SIZE-1 downto 2);
					  if ENABLE_C = 1 then
						  v.sepc(1) := wr_data(1);
					  end if;					
					end if;				
	      when CSR_scause =>	-- Supervisor trap cause
				  if ENABLE_S = 1 then
				    v.scause(3 downto 0) := wr_data(3 downto 0);
					  v.scause(WORD_SIZE-1) := wr_data(WORD_SIZE-1);					
					end if;				
	      when CSR_sbadaddr =>  -- Supervisor bad address
				  if ENABLE_S = 1 and ENABLE_BADADDR = 1 then
						v.sbadaddr := wr_data;
					end if;				
	      when CSR_sip =>	 -- Supervisor interrupt pending
				  if ENABLE_S = 1 then
				    v.mip.SSIP := wr_data(1);
					end if;	
	      when CSR_sptbr =>	 -- Page-table base register
				  if ENABLE_S = 1 then
						if WORD_SIZE = 32 then
							v.sptbr.ASID := wr_data(31 downto 22);
							v.sptbr.PPN := wr_data(21 downto 0);
						else	
--							v.sptbr.ASID := wr_data(63 downto 38);
--							v.sptbr.PPN := wr_data(37 downto 0);	
						end if;	
					end if;				
	      when CSR_hstatus =>
	      when CSR_hedeleg =>
	      when CSR_hideleg =>
	      when CSR_hie =>
	      when CSR_htvec =>
	      when CSR_hscratch =>
	      when CSR_hepc =>
	      when CSR_hcause =>
	      when CSR_hbadaddr =>
	      when CSR_hip =>
	      when CSR_TBD =>
	      when CSR_mvendorid =>
	      when CSR_marchid =>
	      when CSR_mimpid =>
	      when CSR_mhartid =>
	      when CSR_mstatus =>		-- Machine mode status register
				  tlb_flush <= '1';
				  CSR_set_mstatus(status, wr_data);
					v.mstatus.MIE := status.MIE;
					v.mstatus.MPIE := status.MPIE;
	        if ENABLE_U = 1 then
	          v.mstatus.MPRV := status.MPRV;
	          v.mstatus.MPP := trim_privilege(status.MPP);
					else -- TODO
						v.mstatus.MPP := status.MPP;
	        end if;
					if ENABLE_S = 1 and ENABLE_MMU = 1 then
						v.mstatus.MXR := status.MXR;
	          v.mstatus.PUM := status.PUM;
	          v.mstatus.SPP := status.SPP;
	          v.mstatus.SPIE := status.SPIE;
	          v.mstatus.SIE := status.SIE;
	        end if;					
          if ENABLE_MMU = 1 then
						if status.VM = VM_MBARE or status.VM = VM_SV32 then
						  v.mstatus.VM := status.VM;
						end if;	
          end if;
          if ENABLE_MMU = 1 or ENABLE_F = 1 or ENABLE_D = 1 then
						v.mstatus.FS := status.FS;					
					end if;	
	      when CSR_misa =>
	      when CSR_medeleg =>	 -- Machine exception delegation register
			    if ENABLE_S = 1 or ENABLE_U = 1 then
				    v.medeleg := wr_data and get_delegable_except_mask;
				  end if;				
	      when CSR_mideleg =>	 -- Machine interrupt delegation register
			    if ENABLE_S = 1 or ENABLE_U = 1 then
				    v.mideleg := wr_data and get_delegable_irq_mask;
				  end if;				
	      when CSR_mie =>	 -- Machine interrupt-enable register
				  CSR_set_ie(v.mie, wr_data and get_supported_irq_mask);
	      when CSR_mtvec =>			-- Machine Trap-Vector Base-Address Register
				  v.mtvec(WORD_SIZE-1 downto 2) := wr_data(WORD_SIZE-1 downto 2);
	      when CSR_mscratch =>  -- Machine scratch register
					v.mscratch := wr_data;
	      when CSR_mepc =>	 -- Machine exception program counter
				  v.mepc(WORD_SIZE-1 downto 2) := wr_data(WORD_SIZE-1 downto 2);
					if ENABLE_C = 1 then
						v.mepc(1) := wr_data(1);
					end if;	
	      when CSR_mcause =>  -- Machine trap cause
				  v.mcause(3 downto 0) := wr_data(3 downto 0);
					v.mcause(WORD_SIZE-1) := wr_data(WORD_SIZE-1);
	      when CSR_mbadaddr =>
				  if ENABLE_BADADDR = 1 then
				    v.mbadaddr := wr_data;
					end if;	
	      when CSR_mip =>	 -- Machine interrupt pending
				  if ENABLE_S = 1 then
						v.mip.SSIP := wr_data(1);
						v.mip.STIP := wr_data(5);
					end if;	
	      when CSR_mbase =>
	      when CSR_mbound =>
	      when CSR_mibase =>
	      when CSR_mibound =>
	      when CSR_mdbase =>
	      when CSR_mdbound =>
	      when CSR_mcycle =>  -- Machine cycle counter
			    if ENABLE_MCYCLE = 1 then
					  v.mcycle(WORD_SIZE-1 downto 0) := wr_data;
				  end if;				  
	      when CSR_minstret =>  -- Machine instructions-retired counter
			    if ENABLE_MINSTRET = 1 then
					  v.minstret(WORD_SIZE-1 downto 0) := wr_data;
				  end if;				
	      when CSR_mhpmcounter3 =>
	      when CSR_mhpmcounter4 =>
	      when CSR_mhpmcounter31 =>
	      when CSR_mcycleh =>	 -- Upper 32 bits of mcycle, RV32I only
			    if ENABLE_MCYCLE = 1 and WORD_SIZE = 32 then
					  v.mcycle(63 downto WORD_SIZE) := wr_data;
				  end if;				
	      when CSR_minstreth =>	 -- Upper 32 bits of minstret, RV32I only
			    if ENABLE_MINSTRET = 1 and WORD_SIZE = 32 then
					  v.minstret(63 downto WORD_SIZE) := wr_data;
				  end if;				
	      when CSR_mhpmcounter3h =>
	      when CSR_mhpmcounter4h =>
	      when CSR_mhpmcounter31h =>
	      when CSR_mucounteren =>
	      when CSR_mscounteren =>
	      when CSR_mhcounteren =>
	      when CSR_mhpmevent3 =>
	      when CSR_mhpmevent4 =>
	      when CSR_mhpmevent31 =>
	      when CSR_tselect =>
	      when CSR_tdata1 =>
	      when CSR_tdata2 =>
	      when CSR_tdata3 =>
	      when CSR_dcsr =>
				  if ENABLE_DEBUG = 1 then
						CSR_dcsr_write(v.dcsr, wr_data);
					end if;				
	      when CSR_dpc =>
				  if ENABLE_DEBUG = 1 then
						v.dpc := wr_data;
					end if;
	      when CSR_dscratch	=>
				  if ENABLE_DEBUG_ROM1 = 1 then
						v.dscratch := wr_data;
					end if;				
				when CSR_busstate =>
				  if ENABLE_DEBUG_ROM2 = 1 then
						v.busstate.complete := wr_data(2);
					end if;				
        when CSR_busaddress =>
				  if ENABLE_DEBUG_ROM2 = 1 then
						null;
					end if;
        when CSR_busdata =>
				  if ENABLE_DEBUG_ROM2 = 1 then
						v.busdata := wr_data;
					end if;
	      when CSR_dpc0 =>
				  if ENABLE_DEBUG_ROM2 = 1 then
						v.dpc := wr_data;
					end if;					
        when CSR_dscratch0 =>
				  if ENABLE_DEBUG_ROM2 = 1 then
						v.dscratch0 := wr_data;
					end if;				
        when CSR_dscratch1 =>
				  if ENABLE_DEBUG_ROM2 = 1 then
						v.dscratch1 := wr_data;
					end if;				
        when CSR_ccsr =>
				  if ENABLE_DEBUG = 1 then
						CSR_ccsr_write(v.ccsr, wr_data);
					end if;				
        when CSR_cdtmaddress =>
				  if ENABLE_DEBUG = 1 then
--					v.cdtmaddress.dtmaddress := wr_data(CSR_regs.cdtmaddress.dtmaddress'range);
						v.cdtmaddress.cdisable := wr_data(0);
					end if;				
				when others =>  null;			
		  end case;			
		end procedure CSR_write;	
	begin												 
		v := CSR_regs;
		epc := (others => '0');
		ejmp := '0';	
		v.busstate.complete := '0';  -- complete must be no more than 1 cycle long
		tlb_flush <= csr_in.flush_tlb;
		
		v.dcsr.debugint := dbg_irq;
		v.mip.mtip := mtip;
		v.mip.msip := msip;
		v.mip.meip := meip;		
		irq_raised := '0';
		irq_cause := (others => '0');			
		if MEM_AL.ready = '1' then
		  do_interrupts(irq_raised, irq_cause);
		  intr := irq_raised;
		end if;	
		if MEM_AL.except_cmd.raised = '1' then
			irq_raised := '1';
			intr := MEM_AL.except_cmd.interrupt;
			irq_cause := MEM_AL.except_cmd.cause;
		end if;	
		
		if ENABLE_MCYCLE = 1 and debug_mode = '0' then
			v.mcycle := CSR_regs.mcycle + 1;
		end if;	
		if ENABLE_MINSTRET = 1 and debug_mode = '0' and next_AL_WB.ready = '1' then 
			v.minstret := CSR_regs.minstret + 1;
		end if;	

		if ENABLE_DEBUG = 1 then
      if CSR_regs.dcsr.cause = DCSR_CAUSE_NONE then
        if debug_mode = '0' and dbg_irq = '1' then
          enter_debug_mode(DCSR_CAUSE_DEBUGINT);
        elsif debug_mode = '0' and CSR_regs.dcsr.haltinterrupt = '1' then
          enter_debug_mode(DCSR_CAUSE_HALT);
				end if;					
			end if;	
			case CSR_regs.step_state is
				when step_stepping =>
				  if ID_EX.ready = '1' then
				    v.step_state := step_stepped;
					end if;	
				when step_stepped =>
				  if AL_WB.ready = '1' then
				    if MEM_AL.ready = '1' then
				      v.step_state := step_none;
			        enter_debug_mode(DCSR_CAUSE_STEP);
				    else
					    v.step_state := step_wait_next;
				    end if;						
					end if;
				when step_wait_next =>
				  if MEM_AL.ready = '1' then
				    v.step_state := step_none;
			      enter_debug_mode(DCSR_CAUSE_STEP);
					end if;	
				when others => null;
			end case;	
		end if;		
		
		if irq_raised = '0' then
			if csr_in.wr = '1' then	
				CSR_write(v, csr_in.wr_addr, csr_in.wr_data);
			end if;	
			
			if csr_in.eret = '1' then
				do_eret(csr_in.priv);
			end if;								 
			
			if ENABLE_DEBUG = 1 then
			  if csr_in.dret = '1' then
				  do_dret;
			  end if;			
			end if;	 
		else
      if intr = '1' and irq_cause = INT_DebugTrigger then
			  enter_debug_mode(DCSR_CAUSE_HWBP);
		  else 
		    do_trap(irq_cause, intr);
		  end if;		
		end if;  -- irq_raised = '0'		
		
		if ENABLE_MCYCLE = 1 then
			v.mcycle := CSR_regs.mcycle + 1;
		end if;	
		if ENABLE_MINSTRET = 1 and next_AL_WB.ready = '1' then 
			v.minstret := CSR_regs.minstret + 1;
		end if;	

		csr_rd_data <= CSR_read(CSR_regs, csr_in.rd_addr, dbgm_bus_in);
		
		-- Debug Module access from external bus
		if ENABLE_DEBUG = 1 then
			dbgm_bus_out.readdata <= (others => '0');
			dbgm_bus_out.waitrequest <= '0';
			dbgm_bus_out.readdatavalid <= dbgm_bus_in.chipselect and dbgm_bus_in.read;
			if dbgm_bus_in.chipselect = '1' then 
				case dbgm_bus_in.address is
		      when DBGB_ccsr =>	 -- Component Control and Status
					  if dbgm_bus_in.write = '1' then
							CSR_ccsr_write(v.ccsr, dbgm_bus_in.writedata);
						end if;
						dbgm_bus_out.readdata <= CSR_ccsr_read(CSR_regs.ccsr);
		      when DBGB_cdtmaddress =>  -- DTM Interrupt Address  
					  if dbgm_bus_in.write = '1' then
--					  v.cdtmaddress.dtmaddress := wr_data(CSR_regs.cdtmaddress.dtmaddress'range);
						  v.cdtmaddress.cdisable := dbgm_bus_in.writedata(0);						
						end if;					  
						dbgm_bus_out.readdata(CSR_regs.cdtmaddress.dtmaddress'range) <= CSR_regs.cdtmaddress.dtmaddress;
						dbgm_bus_out.readdata(1) <= CSR_regs.cdtmaddress.wired;
						dbgm_bus_out.readdata(0) <= CSR_regs.cdtmaddress.cdisable;		
	        when DBGB_dcsr =>  -- Debug Control and Status
					  if dbgm_bus_in.write = '1' then
							CSR_dcsr_write(v.dcsr, dbgm_bus_in.writedata);
						end if;
						dbgm_bus_out.readdata <= CSR_dcsr_read(CSR_regs.dcsr);		
		      when DBGB_pcsample =>   -- PC Sample
						dbgm_bus_out.readdata <= MEM_AL.pc;
		      when DBGB_dstuff =>  -- Stuff Instruction
		        null;  -- TODO 
		      when DBGB_djump =>   -- Debug Jump
						null;  -- TODO
--		      when DBGB_dpc	=>  -- PC
--						dbgm_bus_out.readdata <= CSR_regs.dpc;
          when others => 
					  if dbgm_bus_in.address(16) = '1' then	 -- CSR registers
--						  if dbgm_bus_in.write = '1' then
--							  CSR_write(v, dbgm_bus_in.address(15 downto 4), dbgm_bus_in.writedata);	
--							end if;
--							dbgm_bus_out.readdata <= CSR_read(CSR_regs, dbgm_bus_in.address(15 downto 4), dbgm_bus_in);
					  elsif dbgm_bus_in.address(13 downto 6) /= 0 then  -- address >= 0x100
							if ENABLE_DEBUG_ROM2 = 1 then
							  dbgm_bus_out.readdata <= CSR_regs.busdata;
								dbgm_bus_out.waitrequest <= not CSR_regs.busstate.complete;
								dbgm_bus_out.readdatavalid <= dbgm_bus_in.read or CSR_regs.busstate.complete;
							end if;	
						end if;	
				end case;	
			end if;
		end if;	
		
		if ENABLE_HW_BREAKPOINT = 1 then
			hw_bp_csr_in.address <= csr_in.wr_addr;
			hw_bp_csr_in.writedata <= csr_in.wr_data;
			hw_bp_csr_in.write <= csr_in.wr;
			hw_bp_csr_in.read <= csr_in.rd;
			if dbgm_bus_in.chipselect = '1' and dbgm_bus_in.write = '1' and dbgm_bus_in.address(16) = '1' then
			  hw_bp_csr_in.address <= dbgm_bus_in.address(15 downto 4);
			  hw_bp_csr_in.writedata <= dbgm_bus_in.writedata;
			  hw_bp_csr_in.write <= '1';
				hw_bp_csr_in.read <= '0';
			end if;	
			case csr_in.rd_addr is
				when CSR_bpselect | CSR_bpcontrol | CSR_bploaddr | CSR_bphiaddr | CSR_bplodata | CSR_bphidata =>
				 csr_rd_data <= hw_bp_csr_readdata;
				when others => null;			
			end case;			
		end if;	
		
		if ENABLE_TRACE = 1 then
			trace_csr_in.address <= csr_in.wr_addr;
			trace_csr_in.writedata <= csr_in.wr_data;
			trace_csr_in.write <= csr_in.wr;
			case csr_in.rd_addr is
				when CSR_trace | CSR_tbufstart | CSR_tbufend | CSR_tbufwrite =>
				  csr_rd_data <= trace_csr_out.readdata;
				when others => null;			
			end case;	
		end if;
		
		jump_cmd.irq <= ejmp;
		jump_cmd.irq_address <= epc;
		jump_cmd.intr <= intr;
		next_CSR_regs <= v;
	end process csr_proc;	
	
	csr_state: process(reset, clk)
	begin
		if reset = '1' then
			CSR_regs.mode <= PRV_M;	
			CSR_regs.step_state <= step_none;
			CSR_regs.mie <= CSR_ie_init;
			CSR_regs.mip <= CSR_ip_init;
      CSR_regs.mtvec <= (others => '0');
      CSR_regs.mscratch <= (others => '0');
			if ENABLE_BADADDR = 1	then
        CSR_regs.mbadaddr <= (others => '0');
			end if;	
			CSR_regs.mstatus <= CSR_mstatus_init;
			if ENABLE_S = 1 or ENABLE_U = 1 then
			  CSR_regs.medeleg <= (others => '0');
			  CSR_regs.mideleg <= (others => '0');
			end if;
      CSR_regs.mepc <= (others => '0');
			CSR_regs.mcause <= (others => '0');
			if ENABLE_MCYCLE = 1 then
				CSR_regs.mcycle <= (others => '0');
			end if;
			if ENABLE_MINSTRET = 1 then
				CSR_regs.minstret <= (others => '0');
			end if;	
			if ENABLE_S = 1 then
			  CSR_regs.sptbr <= CSR_sptbr_init;
        CSR_regs.stvec <= (others => '0');
        CSR_regs.sscratch <= (others => '0');
				CSR_regs.sepc <= (others => '0');
				CSR_regs.scause <= (others => '0');
				if ENABLE_BADADDR = 1 then
          CSR_regs.sbadaddr <= (others => '0');				
				end if;	
			end if;	 
			if ENABLE_DEBUG = 1 then
				CSR_regs.ccsr.authenificated <= '1';
				CSR_regs.ccsr.authbusy <= '0';
				CSR_regs.ccsr.ndreset <= '0';
				CSR_regs.ccsr.fullreset <= '0';
				CSR_regs.ccsr.stopcycle <= '1';
				CSR_regs.ccsr.stoptime <= '0';
				CSR_regs.ccsr.frozen <= '0';
				CSR_regs.ccsr.freezesup <= '0';
				CSR_regs.ccsr.freeze <= '0';
				CSR_regs.ccsr.freezeresume <= '0';
				CSR_regs.ccsr.halted <= '0';
				CSR_regs.ccsr.haltsup <= '0';
				CSR_regs.ccsr.halt <= '0';
				CSR_regs.ccsr.resume <= '0';
				CSR_regs.ccsr.interrupt <= (others => '0');
				
				CSR_regs.dcsr.pcsample <= '0';
				CSR_regs.dcsr.haltinterrupt <= halt;
				CSR_regs.dcsr.debugint <= '0';
				CSR_regs.dcsr.xdebugver <= "01";
				CSR_regs.dcsr.hwbpcount <= CONV_STD_LOGIC_VECTOR(INSN_BP_NUM+DATA_BP_NUM, 12);
				CSR_regs.dcsr.debug <= '0';	
				CSR_regs.dcsr.step <= '0';
				CSR_regs.dcsr.cause <= "000";
				CSR_regs.dcsr.ebreakm <= '0';
				CSR_regs.dcsr.ebreakh <= '0';
				CSR_regs.dcsr.ebreaks <= '0';
				CSR_regs.dcsr.ebreaku <= '0';
				
				CSR_regs.cdtmaddress.dtmaddress <= (others => '0');
				CSR_regs.cdtmaddress.wired <= '0';
				CSR_regs.cdtmaddress.cdisable <= '1';
			end if;
			if ENABLE_DEBUG_ROM2 = 1 then
				CSR_regs.busstate.complete <= '0';
			end if;	
		elsif rising_edge(clk) then	
			CSR_regs <= next_CSR_regs;
			csr_rd_data_reg <= csr_rd_data;
		end if;	
	end process csr_state;	
	
	dbgm_bus: if ENABLE_DEBUG = 1 generate
		dbgm_bus_in.address(16 downto log2(WORD_SIZE/8)) <= avs_dbgm_address;
		dbgm_bus_in.address(log2(WORD_SIZE/8)-1 downto 0) <= (others => '0');
		dbgm_bus_in.chipselect <= avs_dbgm_chipselect;
		dbgm_bus_in.read <= avs_dbgm_read;
		dbgm_bus_in.write <= avs_dbgm_write;
		dbgm_bus_in.writedata <= avs_dbgm_writedata;
		avs_dbgm_readdata <= dbgm_bus_out.readdata;
		avs_dbgm_waitrequest <= dbgm_bus_out.waitrequest;
		avs_dbgm_readdatavalid <= dbgm_bus_out.readdatavalid;		
	end generate dbgm_bus;	
	dbgm_no_bus: if ENABLE_DEBUG = 0 generate
		dbgm_bus_in.chipselect <= '0';
		dbgm_bus_in.read <= '0';
		dbgm_bus_in.write <= '0';
		avs_dbgm_readdata <= (others => '0');
		avs_dbgm_waitrequest <= '0';
		avs_dbgm_readdatavalid <= '0';
	end generate dbgm_no_bus;	
	
	debug_breakpoints: if ENABLE_HW_BREAKPOINT = 1 generate
	bps1: hw_breakpoints
	generic	map
	(
	  INSN_BP_NUM => INSN_BP_NUM,
		DATA_BP_NUM => DATA_BP_NUM,
		ENABLE_FULL_READ => 1
	) 
  port map
  (
	  clk => clk,
		reset => reset,
		din => hw_bp_in,
		dout => hw_bp_out,
		csr_in => hw_bp_csr_in,
		readdata => hw_bp_csr_readdata
	);	
	
	hw_bp_in.pc <= IF_ID.pc;
	hw_bp_in.exec <= IF_ID.ready32;  -- TODO "C"
	hw_bp_in.load <= EX_MEM.mem_rd;
	hw_bp_in.store <= EX_MEM.mem_wr;
	hw_bp_in.address <= EX_MEM.mem_address;
	hw_bp_in.data <= EX_MEM.mem_wr_data;
	hw_bp_in.bytelane <= EX_MEM.mem_byteenable;
	
  end generate debug_breakpoints;
	
	debug_no_breakpoints: if ENABLE_HW_BREAKPOINT = 0 generate
	hw_bp_out.pc_match <= '0';
	hw_bp_out.mem_match <= '0';
	hw_bp_out.match <= '0';
	hw_bp_out.raise_exception_exec  <= '0';
	hw_bp_out.enter_debug_mode_exec <= '0';	
	hw_bp_out.raise_exception_mem  <= '0';
	hw_bp_out.enter_debug_mode_mem <= '0';	
	hw_bp_out.start_tracing <= '0';
	hw_bp_out.stop_tracing <= '0';
	hw_bp_out.emit_trace_data <= '0';		
	end generate debug_no_breakpoints;	
	
	trace_module: if ENABLE_TRACE = 1 generate
  trace1: trace
	generic	map
	(
	  FIFO_SIZE => TRACE_FIFO_SIZE,
		FIFO_WIDTH => TRACE_FIFO_WIDTH,
		INIT_BUF_START => TRACE_INIT_BUF_START,
		INIT_BUF_END => TRACE_INIT_BUF_END
	)
  port map
  (
	  clk => clk,
		reset => reset,
		
		din => trace_in,
		dout => trace_out,
		cmd_in => trace_cmd_in,
		csr_in => trace_csr_in,
    csr_out => trace_csr_out,
		bus_in => trace_bus_in,
    bus_out => trace_bus_out
  );	
	
	avm_trace_address <= trace_bus_out.address;
	avm_trace_write <= trace_bus_out.write;
	avm_trace_writedata <= trace_bus_out.writedata;
	avm_trace_burstcount <= trace_bus_out.burstcount;
	trace_bus_in.waitrequest <= avm_trace_waitrequest;		
	
	trace_in.pc <= MEM_AL.pc;
	trace_in.address <= MEM_AL.mem_address;
	trace_in.readdata <= next_AL_WB.reg_wr_data;
	trace_in.writedata <= MEM_AL.mem_wr_data;
	
	timestamp: if ENABLE_MCYCLE = 1 generate
		trace_in.timestamp <= CSR_regs.mcycle;
	end generate;
	timestamp_no: if ENABLE_MCYCLE = 0 generate
		trace_in.timestamp <= (others => '0');
	end generate;	
	
	trace_in.priv <= CSR_regs.mode;
	trace_in.ie <= get_ie(CSR_regs, CSR_regs.mode);
	
	trace_cmd_in.start <= hw_bp_out.start_tracing;
	trace_cmd_in.stop <= hw_bp_out.stop_tracing;
	
	trace_proc: process(MEM_AL, jump_cmd, csr_in)
	begin
		trace_in.event <= trace_evt_none;	
		trace_in.branch_taken <= '0';
		if jump_cmd.irq = '1' then
			trace_in.branch_target <= jump_cmd.irq_address;
			if jump_cmd.intr = '1' then
				trace_in.event <= trace_evt_intr;
			else	
				trace_in.event <= trace_evt_trap;
			end if;	
		else
			trace_in.branch_target <= MEM_AL.branch_target;	
			case MEM_AL.branch_oper is
		    when br_eq | br_ge |  br_geu | br_lt | br_ltu | br_ne =>
				  trace_in.event <= trace_evt_branch;
					trace_in.branch_taken <= MEM_AL.branch_taken;
		    when br_jal => 
				  if MEM_AL.reg_wr = '1' then
					  trace_in.event <= trace_evt_jal;
					else
						trace_in.event <= trace_evt_branch;
						trace_in.branch_taken <= '1';
					end if;	
		    when br_jalr =>
					trace_in.event <= trace_evt_jalr;
				when br_none =>
				  if MEM_AL.mem_rd = '1' then
						trace_in.event <= trace_evt_load;					
					end if;
				  if MEM_AL.mem_wr = '1' then
						trace_in.event <= trace_evt_store;
					end if;
					if csr_in.wr = '1' then
						trace_in.event <= trace_evt_csr_write;
					end if;	
			end case;	
		end if;
	end process trace_proc;	
		
	end generate trace_module;
	
	trace_no_module: if ENABLE_TRACE = 0 generate
		trace_out.stall <= '0';
	end generate trace_no_module;	
	
	pc <= IF_ID.pc;
	pc_valid <= IF_ID.ready32;
end behaviour;
