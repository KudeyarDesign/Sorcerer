-- Project Sorcerer - RISC-V core
--
-- Special Register (CSR) constants and types
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

package csr is				
	--  ************** CSR Registers  ********************
	
	-- *************** User Trap Setup ********************
	
	-- User status register.
  constant CSR_ustatus : std_logic_vector(11 downto 0) := x"000";   
	-- User interrupt-enable register.
  constant CSR_uie : std_logic_vector(11 downto 0) := x"004"; 	 
  -- User trap handler base address.	
  constant CSR_utvec : std_logic_vector(11 downto 0) := x"005";  	
	
	
	-- **************** User Trap Handling ****************
	
	-- Scratch register for user trap handlers.
  constant CSR_uscratch : std_logic_vector(11 downto 0) := x"040"; 
  -- User exception program counter.
  constant CSR_uepc : std_logic_vector(11 downto 0) := x"041";  	 
  -- User trap cause.
  constant CSR_ucause : std_logic_vector(11 downto 0) := x"042";	 
  -- User bad address.
  constant CSR_ubadaddr : std_logic_vector(11 downto 0) := x"043";  
  -- User interrupt pending.
  constant CSR_uip : std_logic_vector(11 downto 0) := x"044";  	

	-- **************** User Floating-Point CSRs *************
	
	-- Floating-Point Accrued Exceptions.
  constant CSR_fflags : std_logic_vector(11 downto 0) := x"001"; 
	-- Floating-Point Dynamic Rounding Mode.
  constant CSR_frm : std_logic_vector(11 downto 0) := x"002"; 	
	-- Floating-Point Control and Status Register (frm + fflags).
  constant CSR_fcsr : std_logic_vector(11 downto 0) := x"003"; 	

	-- **************** User Counter/Timers	*************
  -- Cycle counter for RDCYCLE instruction.	
  constant CSR_cycle : std_logic_vector(11 downto 0) := x"C00"; 
  -- Timer for RDTIME instruction.
  constant CSR_time : std_logic_vector(11 downto 0) := x"C01"; 	
  -- Instructions-retired counter for RDINSTRET instruction.
  constant CSR_instret : std_logic_vector(11 downto 0) := x"C02"; 		
  -- Performance-monitoring counter.
  constant CSR_hpmcounter3 : std_logic_vector(11 downto 0) := x"C03";  
  -- Performance-monitoring counter.
  constant CSR_hpmcounter4 : std_logic_vector(11 downto 0) := x"C04"; 
	
	-- ... TODO	
	
	-- Performance-monitoring counter.
  constant CSR_hpmcounter31 : std_logic_vector(11 downto 0) := x"C1F";
  -- Upper 32 bits of cycle, RV32I only.
  constant CSR_cycleh : std_logic_vector(11 downto 0) := x"C80"; 
  -- Upper 32 bits of time, RV32I only.
  constant CSR_timeh : std_logic_vector(11 downto 0) := x"C81"; 	 
  -- Upper 32 bits of instret, RV32I only.
  constant CSR_instreth : std_logic_vector(11 downto 0) := x"C82"; 		 
  -- Upper 32 bits of hpmcounter3, RV32I only.
  constant CSR_hpmcounter3h : std_logic_vector(11 downto 0) := x"C83"; 
  -- Upper 32 bits of hpmcounter4, RV32I only.
  constant CSR_hpmcounter4h : std_logic_vector(11 downto 0) := x"C84"; 
	
	-- ... TODO
	
  -- Upper 32 bits of hpmcounter31, RV32I only.
  constant CSR_hpmcounter31h : std_logic_vector(11 downto 0) := x"C9F"; 
	
	-- **************** Supervisor Trap Setup ****************	 
	
	-- Supervisor status register.
  constant CSR_sstatus : std_logic_vector(11 downto 0) := x"100"; 
	-- Supervisor exception delegation register.
  constant CSR_sedeleg : std_logic_vector(11 downto 0) := x"102"; 
	-- Supervisor interrupt delegation register.
  constant CSR_sideleg : std_logic_vector(11 downto 0) := x"103"; 
	-- Supervisor interrupt-enable register.
  constant CSR_sie : std_logic_vector(11 downto 0) := x"104"; 
	-- Supervisor trap handler base address.
  constant CSR_stvec : std_logic_vector(11 downto 0) := x"105"; 	
	
	-- **************** Supervisor Trap Handling ****************
	
  -- Scratch register for supervisor trap handlers.	
  constant CSR_sscratch : std_logic_vector(11 downto 0) := x"140"; 
	-- Supervisor exception program counter.
  constant CSR_sepc : std_logic_vector(11 downto 0) := x"141";
	-- Supervisor trap cause.
  constant CSR_scause : std_logic_vector(11 downto 0) := x"142"; 
	-- Supervisor bad address.
  constant CSR_sbadaddr : std_logic_vector(11 downto 0) := x"143"; 
	-- Supervisor interrupt pending.
  constant CSR_sip : std_logic_vector(11 downto 0) := x"144"; 	
	
	-- **************** Supervisor Protection and Translation ****************
	
	-- Page-table base register.
  constant CSR_sptbr : std_logic_vector(11 downto 0) := x"180"; 
	
	
	-- **************** Hypervisor Trap Setup ****************
	
	-- Hypervisor status register.
  constant CSR_hstatus : std_logic_vector(11 downto 0) := x"200"; 
	-- Hypervisor exception delegation register.
  constant CSR_hedeleg : std_logic_vector(11 downto 0) := x"202"; 
	-- Hypervisor interrupt delegation register.
  constant CSR_hideleg : std_logic_vector(11 downto 0) := x"203"; 
	-- Hypervisor interrupt-enable register.
  constant CSR_hie : std_logic_vector(11 downto 0) := x"204"; 	 
	-- Hypervisor trap handler base address.
  constant CSR_htvec : std_logic_vector(11 downto 0) := x"205"; 	
	
	-- **************** Hypervisor Trap Handling ****************
	
	-- Scratch register for hypervisor trap handlers.
  constant CSR_hscratch : std_logic_vector(11 downto 0) := x"240"; 
	-- Hypervisor exception program counter.
  constant CSR_hepc : std_logic_vector(11 downto 0) := x"241"; 	
	-- Hypervisor trap cause.
  constant CSR_hcause : std_logic_vector(11 downto 0) := x"242"; 	
	-- Hypervisor bad address.
  constant CSR_hbadaddr : std_logic_vector(11 downto 0) := x"243"; 
	-- Hypervisor interrupt pending.
  constant CSR_hip : std_logic_vector(11 downto 0) := x"244"; 	
	
	-- **************** Hypervisor Protection and Translation	**************** 
	
	-- TBD
  constant CSR_TBD : std_logic_vector(11 downto 0) := x"280";	
	
	-- **************** Machine Information Registers ****************
	
	-- Vendor ID.
  constant CSR_mvendorid : std_logic_vector(11 downto 0) := x"F11"; 
	-- Architecture ID.
  constant CSR_marchid : std_logic_vector(11 downto 0) := x"F12"; 
	-- Implementation ID.
  constant CSR_mimpid : std_logic_vector(11 downto 0) := x"F13";  
	-- Hardware thread ID.
  constant CSR_mhartid : std_logic_vector(11 downto 0) := x"F14"; 	
	
	-- **************** Machine Trap Setup ****************
	
	-- Machine status register.
  constant CSR_mstatus : std_logic_vector(11 downto 0) := x"300"; 
	-- ISA and extensions
  constant CSR_misa : std_logic_vector(11 downto 0) := x"301"; 	 
	-- Machine exception delegation register.
  constant CSR_medeleg : std_logic_vector(11 downto 0) := x"302"; 
	-- Machine interrupt delegation register.
  constant CSR_mideleg : std_logic_vector(11 downto 0) := x"303"; 
	-- Machine interrupt-enable register.
  constant CSR_mie : std_logic_vector(11 downto 0) := x"304"; 
	-- Machine trap-handler base address.
  constant CSR_mtvec : std_logic_vector(11 downto 0) := x"305"; 	
	
	-- **************** Machine Trap Handling ****************
	
	-- Scratch register for machine trap handlers.
  constant CSR_mscratch : std_logic_vector(11 downto 0) := x"340"; 							
	-- Machine exception program counter.
  constant CSR_mepc : std_logic_vector(11 downto 0) := x"341"; 
	-- Machine trap cause.
  constant CSR_mcause : std_logic_vector(11 downto 0) := x"342"; 	
	-- Machine bad address.
  constant CSR_mbadaddr : std_logic_vector(11 downto 0) := x"343"; 
	-- Machine interrupt pending.
  constant CSR_mip : std_logic_vector(11 downto 0) := x"344"; 	
	
	-- **************** Machine Protection and Translation ****************
	
	-- Base register.
  constant CSR_mbase : std_logic_vector(11 downto 0) := x"380"; 	
	-- Bound register.
  constant CSR_mbound : std_logic_vector(11 downto 0) := x"381"; 
	-- Instruction base register.
  constant CSR_mibase : std_logic_vector(11 downto 0) := x"382"; 
	-- Instruction bound register.
  constant CSR_mibound : std_logic_vector(11 downto 0) := x"383"; 
	-- Data base register.
  constant CSR_mdbase : std_logic_vector(11 downto 0) := x"384";  
	-- Data bound register.
  constant CSR_mdbound : std_logic_vector(11 downto 0) := x"385"; 	
	
	-- **************** Machine Counter/Timers ****************
	
	-- Machine cycle counter.
  constant CSR_mcycle : std_logic_vector(11 downto 0) := x"B00";	
	-- Machine instructions-retired counter.
  constant CSR_minstret : std_logic_vector(11 downto 0) := x"B02"; 
	-- Machine performance-monitoring counter.
  constant CSR_mhpmcounter3 : std_logic_vector(11 downto 0) := x"B03"; 
	-- Machine performance-monitoring counter.
  constant CSR_mhpmcounter4 : std_logic_vector(11 downto 0) := x"B04"; 
  -- TODO ...													 
	
	-- Machine performance-monitoring counter.
  constant CSR_mhpmcounter31 : std_logic_vector(11 downto 0) := x"B1F"; 
	-- Upper 32 bits of mcycle, RV32I only.
  constant CSR_mcycleh : std_logic_vector(11 downto 0) := x"B80"; 
	-- Upper 32 bits of minstret, RV32I only.
  constant CSR_minstreth : std_logic_vector(11 downto 0) := x"B82"; 
	-- Upper 32 bits of mhpmcounter3, RV32I only.
  constant CSR_mhpmcounter3h : std_logic_vector(11 downto 0) := x"B83"; 
	-- Upper 32 bits of mhpmcounter4, RV32I only.
  constant CSR_mhpmcounter4h : std_logic_vector(11 downto 0) := x"B84"; 
  -- TODO ...	
	-- Upper 32 bits of mhpmcounter31, RV32I only.
  constant CSR_mhpmcounter31h : std_logic_vector(11 downto 0) := x"B9F"; 	

	-- Branch predictor counters
	constant CSR_bpred_counter1: std_logic_vector(11 downto 0) := CSR_mhpmcounter3;
	constant CSR_bpred_counter2: std_logic_vector(11 downto 0) := CSR_bpred_counter1 + 1;	
	constant CSR_bpred_counter3: std_logic_vector(11 downto 0) := CSR_bpred_counter2 + 1;
	constant CSR_bpred_counter4: std_logic_vector(11 downto 0) := CSR_bpred_counter3 + 1;
	-- Upper 32 bits of branch predictor counters
	constant CSR_bpred_counter1h: std_logic_vector(11 downto 0) := CSR_mhpmcounter3h;
	constant CSR_bpred_counter2h: std_logic_vector(11 downto 0) := CSR_bpred_counter1h + 1;	
	constant CSR_bpred_counter3h: std_logic_vector(11 downto 0) := CSR_bpred_counter2h + 1;
	constant CSR_bpred_counter4h: std_logic_vector(11 downto 0) := CSR_bpred_counter3h + 1;	
	-- Number of counters in branch bredictor
	constant CSR_bpred_counter_num: natural := 4;
	
	-- **************** Machine Counter Setup ****************
	
	-- User-mode counter enable.
  constant CSR_mucounteren : std_logic_vector(11 downto 0) := x"320"; 
	-- Supervisor-mode counter enable.
  constant CSR_mscounteren : std_logic_vector(11 downto 0) := x"321"; 
	-- Hypervisor-mode counter enable.
  constant CSR_mhcounteren : std_logic_vector(11 downto 0) := x"322";  
	-- Machine performance-monitoring event selector.
  constant CSR_mhpmevent3 : std_logic_vector(11 downto 0) := x"323"; 
	-- Machine performance-monitoring event selector.
  constant CSR_mhpmevent4 : std_logic_vector(11 downto 0) := x"324";  
  -- TODO ...	
	-- Machine performance-monitoring event selector.
  constant CSR_mhpmevent31 : std_logic_vector(11 downto 0) := x"33F";  

	-- **************** Debug/Trace Registers (shared with Debug Mode) ****************
	
	-- Debug/Trace trigger register select.
  constant CSR_tselect : std_logic_vector(11 downto 0) := x"7A0"; 
	-- First Debug/Trace trigger data register.
  constant CSR_tdata1 : std_logic_vector(11 downto 0) := x"7A1"; 
	-- Second Debug/Trace trigger data register.
  constant CSR_tdata2 : std_logic_vector(11 downto 0) := x"7A2"; 
	-- Third Debug/Trace trigger data register.
  constant CSR_tdata3 : std_logic_vector(11 downto 0) := x"7A3";  

	-- **************** Debug Mode Registers ****************
	
	-- Debug control and status register.
  constant CSR_dcsr : std_logic_vector(11 downto 0) := x"7B0"; 
	-- Debug PC.
  constant CSR_dpc : std_logic_vector(11 downto 0) := x"7B1";  
	-- Debug scratch register.
  constant CSR_dscratch : std_logic_vector(11 downto 0) := x"7B2";  	
	
	
	-- **************** Debug Mode Registers ****************	
	-- Bus State.
  constant CSR_busstate : std_logic_vector(11 downto 0) := x"770";	
	-- Bus Address.
  constant CSR_busaddress : std_logic_vector(11 downto 0) := x"771";
	-- Bus Data.
  constant CSR_busdata : std_logic_vector(11 downto 0) := x"772";
	-- Debug PC.	- TODO conflict with dpc on address 7B1
  constant CSR_dpc0 : std_logic_vector(11 downto 0) := x"773";	
	-- Debug scratch register 0.
  constant CSR_dscratch0 : std_logic_vector(11 downto 0) := x"774";
	-- Debug scratch register 1.
  constant CSR_dscratch1 : std_logic_vector(11 downto 0) := x"775";	
	-- Component Control and Status.
  constant CSR_ccsr : std_logic_vector(11 downto 0) := x"776";	
	-- DTM interrupt address.
  constant CSR_cdtmaddress : std_logic_vector(11 downto 0) := x"777";	

	-- Breakpoint Select
  constant CSR_bpselect : std_logic_vector(11 downto 0) := x"780";
	-- Breakpoint Control
	constant CSR_bpcontrol : std_logic_vector(11 downto 0) := x"781";
	-- Breakpoint Low Address
	constant CSR_bploaddr : std_logic_vector(11 downto 0) := x"782";
	-- Breakpoint High Address 
	constant CSR_bphiaddr : std_logic_vector(11 downto 0) := x"783";
	-- Breakpoint Low Data
	constant CSR_bplodata : std_logic_vector(11 downto 0) := x"784";
	-- Breakpoint High Data
	constant CSR_bphidata : std_logic_vector(11 downto 0) := x"785";
	
	-- **************** Trace Registers ****************	
	
	-- Trace
	constant CSR_trace : std_logic_vector(11 downto 0) := x"788";
	-- Trace Buffer Start
	constant CSR_tbufstart : std_logic_vector(11 downto 0) := x"789";
	-- Trace Buffer End
	constant CSR_tbufend : std_logic_vector(11 downto 0) := x"78A";
	-- Trace Buffer Write
	constant CSR_tbufwrite : std_logic_vector(11 downto 0) := x"78B";	
	
	-- Debug Bus Registers
	-- Component Control and Status
	constant DBGB_ccsr : std_logic_vector(16 downto 0) := CONV_STD_LOGIC_VECTOR(16#00000#, 17);
	-- DTM Interrupt Address
  constant DBGB_cdtmaddress : std_logic_vector(16 downto 0) := CONV_STD_LOGIC_VECTOR(16#00010#, 17);	
	-- Debug Control and Status
  constant DBGB_dcsr : std_logic_vector(16 downto 0) := CONV_STD_LOGIC_VECTOR(16#00020#, 17);	
	-- PC Sample
	constant DBGB_pcsample : std_logic_vector(16 downto 0) := CONV_STD_LOGIC_VECTOR(16#00030#, 17);
	-- Stuff Instruction
	constant DBGB_dstuff : std_logic_vector(16 downto 0) := CONV_STD_LOGIC_VECTOR(16#00100#, 17);
	-- Debug Jump
	constant DBGB_djump : std_logic_vector(16 downto 0) := CONV_STD_LOGIC_VECTOR(16#00110#, 17);
	-- PC
	constant DBGB_dpc : std_logic_vector(16 downto 0) := CONV_STD_LOGIC_VECTOR(16#00120#, 17);
	
	-- CSR_mie
	type CSR_ie_type is record
		MEIE: std_logic;
		HEIE: std_logic;
		SEIE: std_logic;
		UEIE: std_logic;
		MTIE: std_logic;
		HTIE: std_logic;
		STIE: std_logic;
		UTIE: std_logic;
		MSIE: std_logic;
		HSIE: std_logic; 
		SSIE: std_logic; 
		USIE: std_logic;
	end record;	

	constant CSR_ie_init: CSR_ie_type :=
	(
		MEIE => '0',
		HEIE => '0',
		SEIE => '0',
		UEIE => '0',
		MTIE => '0',
		HTIE => '0',
		STIE => '0',
		UTIE => '0',
		MSIE => '0',
		HSIE => '0',
		SSIE => '0',
		USIE => '0'	
	);
	
	function CSR_get_ie(r: CSR_ie_type) return std_logic_vector;
	procedure CSR_set_ie(r: out CSR_ie_type; value: in std_logic_vector(31 downto 0));
	
	-- CSR_mip
	type CSR_ip_type is record
		MEIP: std_logic;
		HEIP: std_logic;
		SEIP: std_logic;
		UEIP: std_logic;
		MTIP: std_logic;
		HTIP: std_logic;
		STIP: std_logic;
		UTIP: std_logic;
		MSIP: std_logic;
		HSIP: std_logic; 
		SSIP: std_logic; 
		USIP: std_logic;
	end record;	

	constant CSR_ip_init: CSR_ip_type :=
	(
		MEIP => '0',
		HEIP => '0',
		SEIP => '0',
		UEIP => '0',
		MTIP => '0',
		HTIP => '0',
		STIP => '0',
		UTIP => '0',
		MSIP => '0',
		HSIP => '0',
		SSIP => '0',
		USIP => '0'	
	);
	
	function CSR_get_ip(r: CSR_ip_type) return std_logic_vector;
	procedure CSR_set_ip(r: out CSR_ip_type; value: in std_logic_vector(31 downto 0));	
	
	-- Privilege levels:
  -- User mode
  constant PRV_U : std_logic_vector(1 downto 0) := "00";
  -- supervisor mode
  constant PRV_S : std_logic_vector(1 downto 0) := "01";
  -- hypervisor mode
  constant PRV_H : std_logic_vector(1 downto 0) := "10";
  -- machine mode
  constant PRV_M : std_logic_vector(1 downto 0) := "11";	
	
	-- mstatus VM[4:0] values:
	-- No translation or protection
  constant VM_MBARE: std_logic_vector(4 downto 0) := "00000"; -- 0
	-- Single base-and-bound
  constant VM_MBB: std_logic_vector(4 downto 0) := "00000";	-- 1
	-- Separate instruction and data base-and-bound.
  constant VM_MBBID: std_logic_vector(4 downto 0) := "00010";	-- 2
	-- Page-based 32-bit virtual addressing.
  constant VM_SV32: std_logic_vector(4 downto 0) := "01000"; --  8
	-- Page-based 39-bit virtual addressing.
  constant VM_SV39: std_logic_vector(4 downto 0) := "01001"; --  9	 
	-- Page-based 48-bit virtual addressing.
  constant VM_SV48: std_logic_vector(4 downto 0) := "01010"; --  10	
	
	type CSR_mstatus_type is record
		SD: std_logic;
    VM: std_logic_vector(4 downto 0);
		MXR: std_logic;
		PUM: std_logic;
		MPRV: std_logic;
		XS: std_logic_vector(1 downto 0);
	  FS: std_logic_vector(1 downto 0);
		MPP: std_logic_vector(1 downto 0);
		HPP: std_logic_vector(1 downto 0);
		SPP: std_logic; 
		MPIE: std_logic; 
		HPIE: std_logic; 
		SPIE: std_logic; 
		UPIE: std_logic; 
		MIE: std_logic; 
		HIE: std_logic; 
		SIE: std_logic; 
		UIE: std_logic;
	end record;
	
	constant CSR_mstatus_init: CSR_mstatus_type :=
	(
	  SD => '0',
    VM => (others => '0'),
		MXR => '0',
		PUM => '0',
		MPRV => '0',
		XS => (others => '0'),
	  FS => (others => '0'),
		MPP => (others => '0'),
		HPP => (others => '0'),
		SPP => '0',
		MPIE => '0',
		HPIE => '0',
		SPIE => '0',
		UPIE => '0',
		MIE => '0',
		HIE => '0',
		SIE => '0',
		UIE => '0'	
	); 
	
	function CSR_get_mstatus(r: CSR_mstatus_type) return std_logic_vector;
	procedure CSR_set_mstatus(r: out CSR_mstatus_type; value: in std_logic_vector(31 downto 0));	
	
	-- TODO 64-bit
	type CSR_sptbr_type is record
		ASID: std_logic_vector(9 downto 0);
		PPN: std_logic_vector(21 downto 0);
	end record;	

	constant CSR_sptbr_init: CSR_sptbr_type :=
	(
	  ASID => (others => '0'),
		PPN => (others => '0')
	);
	
	-- Debuger state cause (field "cause" in "dcsr" register	
	-- Core is not in Debug Mode
  constant DCSR_CAUSE_NONE: std_logic_vector(2 downto 0) := "000";
	-- Software breakpoint was hit
  constant DCSR_CAUSE_SWBP: std_logic_vector(2 downto 0) := "001";
	-- Hardware breakpoint was hit
  constant DCSR_CAUSE_HWBP: std_logic_vector(2 downto 0) := "010";
	-- Halt in ccsr was set
  constant DCSR_CAUSE_DEBUGINT: std_logic_vector(2 downto 0) := "011";
	-- Core single stepped
  constant DCSR_CAUSE_STEP: std_logic_vector(2 downto 0) := "100";
	-- Core halted
  constant DCSR_CAUSE_HALT: std_logic_vector(2 downto 0) := "101";
	
	type CSR_dcsr_type is record
		pcsample: std_logic;
		haltinterrupt: std_logic;
		debugint: std_logic;
		xdebugver: std_logic_vector(1 downto 0);
		hwbpcount: std_logic_vector(11 downto 0);
		prv: std_logic_vector(1 downto 0);
		debug: std_logic;
		step: std_logic;  -- version 0.11
		cause: std_logic_vector(2 downto 0);
		ebreakm: std_logic; 
		ebreakh: std_logic; 
		ebreaks: std_logic; 
		ebreaku: std_logic;
	end record;	

	constant CSR_dcsr_init: CSR_dcsr_type :=
	(
		pcsample => '0',
		haltinterrupt => '0',
		debugint => '0',
		xdebugver => (others => '0'),
		hwbpcount => (others => '0'),
		prv => "00",
		debug => '0',
		step => '0',  -- version 0.11
		cause => (others => '0'),
		ebreakm => '0', 
		ebreakh => '0',
		ebreaks => '0',
		ebreaku	=> '0'
	);	
	
	-- Conversions for debugger only
	function CSR_get_dcsr(r: CSR_dcsr_type) return std_logic_vector;
	procedure CSR_set_dcsr(r: out CSR_dcsr_type; value: in std_logic_vector(31 downto 0));	
	
	type CSR_ccsr_type is record
		authenificated: std_logic;
		authbusy: std_logic;
		ndreset: std_logic;
		fullreset: std_logic;
		stopcycle: std_logic;
		stoptime: std_logic;
		frozen: std_logic;
		freezesup: std_logic;
		freeze: std_logic;
		freezeresume: std_logic;
		halted: std_logic;
		haltsup: std_logic;
		halt: std_logic;
		resume: std_logic;
		interrupt: std_logic_vector(7 downto 0);
	end record;	
	
	type CSR_cdtmaddress_type is record
		dtmaddress: std_logic_vector(PADDRESS_WIDTH-1 downto 2);
		wired: std_logic;
		cdisable: std_logic;
	end record;	
	
	type CSR_busstate_type is record
		complete: std_logic;
	end record;	

	type step_state_type is (step_none, step_stepping, step_stepped, step_wait_next);
	
  type CSR_regs_type is record
		mode: std_logic_vector(1 downto 0);
		step_state: step_state_type;
		
		mstatus: CSR_mstatus_type;
		medeleg: std_logic_vector(WORD_SIZE-1 downto 0);
		mideleg: std_logic_vector(WORD_SIZE-1 downto 0);
		mie: CSR_ie_type;
		mip: CSR_ip_type;
		mtvec : std_logic_vector(WORD_SIZE-1 downto 0);
		mscratch : std_logic_vector(WORD_SIZE-1 downto 0);
    mepc : std_logic_vector(WORD_SIZE-1 downto 0);		
		
		mcause: std_logic_vector(WORD_SIZE-1 downto 0);
		mbadaddr : std_logic_vector(WORD_SIZE-1 downto 0);
		
		mcycle: std_logic_vector(63 downto 0);
		minstret: std_logic_vector(63 downto 0);
		
		sptbr: CSR_sptbr_type;
		
		stvec : std_logic_vector(WORD_SIZE-1 downto 0);
		sscratch : std_logic_vector(WORD_SIZE-1 downto 0);
    sepc : std_logic_vector(WORD_SIZE-1 downto 0);		
		
		scause: std_logic_vector(WORD_SIZE-1 downto 0);
		sbadaddr : std_logic_vector(WORD_SIZE-1 downto 0);
		
		ccsr: CSR_ccsr_type;
		cdtmaddress: CSR_cdtmaddress_type;
		dcsr: CSR_dcsr_type;
		dscratch : std_logic_vector(WORD_SIZE-1 downto 0);
    dpc : std_logic_vector(WORD_SIZE-1 downto 0);
		
		busstate : CSR_busstate_type; 
		busdata : std_logic_vector(WORD_SIZE-1 downto 0); 
    dscratch0 : std_logic_vector(WORD_SIZE-1 downto 0);
    dscratch1 : std_logic_vector(WORD_SIZE-1 downto 0);		
	end record;	
	
end package csr;

package body csr is
  function CSR_get_ie(r: CSR_ie_type) return std_logic_vector is
	begin
		return x"00000" & r.MEIE & r.HEIE & r.SEIE & r.UEIE & r.MTIE & r.HTIE & r.STIE & r.UTIE & r.MSIE & r.HSIE & r.SSIE & r.USIE;		
	end function CSR_get_ie;

  procedure CSR_set_ie(r: out CSR_ie_type; value: std_logic_vector(31 downto 0))	is
	begin
   r.MEIE	:= value(11); 
   r.HEIE	:= value(10); 
   r.SEIE	:= value(9); 
   r.UEIE	:= value(8); 
   r.MTIE	:= value(7); 
   r.HTIE	:= value(6); 
   r.STIE	:= value(5); 
   r.UTIE	:= value(4); 
   r.MSIE	:= value(3); 
   r.HSIE := value(2);
   r.SSIE := value(1);
   r.USIE	:= value(0);
	end procedure CSR_set_ie;
	
  function CSR_get_ip(r: CSR_ip_type) return std_logic_vector is
	begin
		return x"00000" & r.MEIP & r.HEIP & r.SEIP & r.UEIP & r.MTIP & r.HTIP & r.STIP & r.UTIP & r.MSIP & r.HSIP & r.SSIP & r.USIP;		
	end function CSR_get_ip;

  procedure CSR_set_ip(r: out CSR_ip_type; value: in std_logic_vector(31 downto 0))	is
	begin
   r.MEIP	:= value(11); 
   r.HEIP	:= value(10); 
   r.SEIP	:= value(9); 
   r.UEIP	:= value(8); 
   r.MTIP	:= value(7); 
   r.HTIP	:= value(6); 
   r.STIP	:= value(5); 
   r.UTIP	:= value(4); 
   r.MSIP	:= value(3); 
   r.HSIP := value(2);
   r.SSIP := value(1);
   r.USIP	:= value(0);
	end procedure CSR_set_ip;	
	
  function CSR_get_mstatus(r: CSR_mstatus_type) return std_logic_vector is
	begin
		return r.SD & "00" & r.VM & "0000" & r.MXR & r.PUM & r.MPRV & r.XS &
      r.FS & r.MPP & r.HPP & r.SPP & r.MPIE & r.HPIE & r.SPIE & r.UPIE & r.MIE & r.HIE & r.SIE & r.UIE;
	end function CSR_get_mstatus;	
	
	procedure CSR_set_mstatus(r: out CSR_mstatus_type; value: in std_logic_vector(31 downto 0)) is
	begin
		r.SD := value(31);
    r.VM := value(28 downto 24); -- TODO 
		r.MXR := value(19);
		r.PUM := value(18);
		r.MPRV := value(17);
		r.XS := value(16 downto 15);
	  r.FS := value(14 downto 13);
		r.MPP := value(12 downto 11);
		r.HPP := value(10 downto 9);
		r.SPP := value(8);
		r.MPIE := value(7); 
		r.HPIE := value(6);
		r.SPIE := value(5); 
		r.UPIE := value(4); 
		r.MIE := value(3); 
		r.HIE := value(2); 
		r.SIE := value(1);
		r.UIE := value(0);		
	end procedure CSR_set_mstatus;
	
	function CSR_get_dcsr(r: CSR_dcsr_type) return std_logic_vector is
	begin
		return r.pcsample & "0" & r.xdebugver & r.hwbpcount &
		  r.ebreakm & r.ebreakh & r.ebreaks & r.ebreaku &
			"00" &
		  r.debug & r.cause & r.debugint & "0" &
			r.haltinterrupt & r.step & r.prv;
	end function CSR_get_dcsr;
	
	procedure CSR_set_dcsr(r: out CSR_dcsr_type; value: in std_logic_vector(31 downto 0)) is
	begin
		r.pcsample := value(31);
		r.xdebugver := value(29 downto 28);
		r.hwbpcount := value(27 downto 16);
		r.ebreakm := value(15);
		r.ebreakh := value(14);
		r.ebreaks := value(13);
		r.ebreaku := value(12);		
		r.debug := value(9);
		r.cause := value(8 downto 6);
		r.debugint := value(5);
		r.haltinterrupt := value(3);		
		r.step := value(2);
		r.prv := value(1 downto 0);
	end procedure CSR_set_dcsr;	
end package body csr;
