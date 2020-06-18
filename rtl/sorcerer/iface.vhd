library IEEE;
use IEEE.STD_LOGIC_1164.all;
use work.config.all;
use work.pipeline_iface.all;
use work.utils.all;
use work.csr.all;

package iface is
	
component decoder_rv32im is 
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
end	component;

component decoder_rv32c is
	port
	(
	  insn: in std_logic_vector(15 downto 0);
		pc: in std_logic_vector(31 downto 0);
		dout : out insn_decoder_out_type
	);	
end	component;

component decompressor_rv32c is
	port
	(
	  insn: in std_logic_vector(15 downto 0);
		insn32: out std_logic_vector(31 downto 0);
		valid: out std_logic
	);	
end	component;

component gpr is
	generic
	(
		REGISTER_NUM: integer range 8 to 32 := 32
	);
	port
	(
		clk : in std_logic;
		reset : in std_logic;
		din: in gpr_in_type;
		dout: out gpr_out_type
	);	
end	component;

component ram_block is
	generic
	(
	  SIZE: natural := 1024;		-- size in bytes
		DATA_WIDTH: natural range 8 to 64 := 32  -- must be 2 ** N
	);
  port
  (
    clk : in std_logic;
	  rdaddress: in std_logic_vector(ceil_log2(SIZE*8/DATA_WIDTH)-1 downto 0);
	  wraddress: in std_logic_vector(ceil_log2(SIZE*8/DATA_WIDTH)-1 downto 0);
	  data : in std_logic_vector(DATA_WIDTH-1 downto 0);
	  wren: in std_logic;
	  byteen: in std_logic_vector(DATA_WIDTH/8-1 downto 0);
	 
	  q : out std_logic_vector(DATA_WIDTH-1 downto 0)
  );	
end component;

component mul16x16 is
	port
	(
		dataa_0: in std_logic_vector(15 downto 0) := (others => '0');
		datab_0: in std_logic_vector(15 downto 0) := (others => '0');
		signa: in std_logic := '0';
		signb: in std_logic := '0';
		result: out std_logic_vector(31 downto 0)
	);
end component;

component mul32x32 is
	port
	(	 
		clk : in std_logic;
		reset : in std_logic;	  
	  din: in mul_in_type;
		dout: out mul_out_type
	);	
end component;

component div32x32 is
	port
	(	 
		clk : in std_logic;
		reset : in std_logic;	  
	  din: in div_in_type;
		dout: out div_out_type
	);	
end	component;

component barrel_shifter is
	port
	(
	  a: in std_logic_vector(WORD_SIZE-1 downto 0);
		amount: in std_logic_vector(7 downto 0);
		right: in std_logic;
		sign: in std_logic;
		result: out std_logic_vector(WORD_SIZE-1 downto 0)
	);
end component;

component alu is
	generic
	(
	  ENABLE_SHIFTER: natural range 0 to 1 := 0
	);	
	port
	(
	  din: in alu_in_type;
		dout: out alu_out_type
	);
end component;

component amoalu is
	port
	(
	  din: in amoalu_in_type;
		result: out std_logic_vector(WORD_SIZE-1 downto 0)
	);	
end component;

component icache is 
	generic
	(
	  SIZE: natural := 16384;		-- size in bytes
		ASSOCIATIVITY: natural range 1 to 8 := 2;
		LINE_SIZE: natural := 8;  -- instruction words per line (with ICACHE_FETCH_WIDTH)
		ENABLE_MMU: natural range 0 to 1 := 1;
		SYNTH: natural range 0 to 1 := 0
	);
  port
  (
	  clk : in std_logic;
		reset : in std_logic;
		din: in icache_fetch_in_type;
		dout: out icache_fetch_out_type;
		cmd_in: in icache_cmd_in_type;
--		cmd_out: out cache_cmd_out_type;
		mmu_in: in icache_mmu_in_type;
		mmu_out: out icache_mmu_out_type;
		bus_in: in icache_bus_in_type;
		bus_out: out icache_bus_out_type		
	);	
end component;

component dcache is
	generic
	(
	  SIZE: natural := 16384;		-- size in bytes
		ASSOCIATIVITY: natural range 1 to 8 := 2;
		LINE_SIZE: natural := 8;  -- instruction words per line (with ICACHE_FETCH_WIDTH)
		ENABLE_MMU: natural range 0 to 1 := 1;
		ENABLE_A: natural range 0 to 1 := 0;
		SYNTH: natural range 0 to 1 := 1
	);
  port
  (
	  clk : in std_logic;
		reset : in std_logic;
		din: in dcache_mem_in_type;
		dout: out dcache_mem_out_type;
		cmd_in: in dcache_cmd_in_type;
--		cmd_out: out cache_cmd_out_type;
		mmu_in: in dcache_mmu_in_type;
		mmu_out: out dcache_mmu_out_type;
		bus_in: in dcache_bus_in_type;
		bus_out: out dcache_bus_out_type		
	);	
end component;

component bht is
	generic
	(
		ENABLE_C: natural range 0 to 1 := 0      -- Compressed extension
	);
	port
	(
		clk : in std_logic;
		reset : in std_logic;
		
		din: in bht_in_type;
		dout: out bht_data_type;
		upd_in: in bht_update_type
	);	
end component;

component ras is
	generic
	(
	  DEPTH: natural range 1 to 256 := 8;
		ENABLE_C: natural range 0 to 1 := 0    -- Compressed extension
	);
	port
	(
		clk : in std_logic;
		reset : in std_logic;	

		din: in ras_in_type;
		dout: out ras_out_type
	);	
end component;

component btb is
	generic
	(
		ENABLE_C: natural range 0 to 1 := 0      -- Compressed extension
	);
	port
	(
		clk : in std_logic;
		reset : in std_logic;
		
		din: in btb_in_type;
		dout: out btb_out_type;
		upd_in: in btb_update_type
	);	
end component;

component branch_predictor is
	generic
	(
	  ENABLE_C: natural range 0 to 1 := 0;     -- Compressed extension
		ENABLE_BPRED_STAT: natural range 0 to 1 := 0  -- Branch prediction statistics
	);
	port
	(
		clk : in std_logic;
		reset : in std_logic;
		
		din: in bpred_in_type;
		dout: out bpred_out_type;
		upd_in: in bpred_update_type;
		
		csr_in: in extern_csr_in_type;
		readdata: out std_logic_vector(WORD_SIZE-1 downto 0)		
	);	
end component;

component ibuffer_core is
	generic
	(
	  SLICE_WIDTH: natural range 8 to 32 := 16;     -- Bit width of a slice (data + auxiliary info)
		FETCH_SLICE_NUM: natural range 1 to 16 := 2;  -- Number of slices in one instruction fetch word
		BUFFER_DEPTH: natural range 1 to 32 := 4;     -- Number of slices in buffer
		INSN_SLICE_NUM: natural range 1 to 32 := 2;   -- Number of slices in buffer output, maximum number of slices per instruction
		UNREGISTERED_OUT: natural range 0 to 1 := 0   -- If 1, instruction output is unregistered, otherwise registered
	);
	port
	(
		clk : in std_logic;
		reset : in std_logic;
		
		data: in std_logic_2d_type(0 to FETCH_SLICE_NUM-1, SLICE_WIDTH-1 downto 0);
		data_valid: in std_logic_vector(FETCH_SLICE_NUM-1 downto 0);
		flush: in std_logic;
		hard_stall: in std_logic;
		insn_size: in std_logic_vector(ceil_log2(INSN_SLICE_NUM+1)-1 downto 0);
		
		busy: out std_logic;
		insn: out std_logic_2d_type(0 to INSN_SLICE_NUM-1, SLICE_WIDTH-1 downto 0);  
		insn_valid: out std_logic_vector(INSN_SLICE_NUM-1 downto 0)
	);	
end component;

component ibuffer is
	generic
	(
	  BUFFER_DEPTH: natural range 1 to 32 := 4; -- Number of 16-bit slices in buffer
    UNREGISTERED_OUT: natural range 0 to 1 := 0   -- If 1, instruction output is unregistered, otherwise registered	
	);
	port
	(
		clk : in std_logic;
		reset : in std_logic;
		
		din: in ibuffer_in_type;
		dout: out ibuffer_out_type
	);	
end component;

component lru is
	generic
	(
		SIZE: natural range 4 to 64 := 4
	);
  port
  (
	  clk : in std_logic;
		reset : in std_logic;
		touch: in std_logic;
		touch_pos: in std_logic_vector(ceil_log2(SIZE)-1 downto 0);
		last_pos: out std_logic_vector(ceil_log2(SIZE)-1 downto 0)
	);	
end component;

component pseudo_lru is
	generic
	(
		SIZE: natural range 4 to 256 := 16
	);
  port
  (
	  clk : in std_logic;
		reset : in std_logic;
		touch: in std_logic;
		touch_pos: in std_logic_vector(log2(SIZE)-1 downto 0);
		last_pos: out std_logic_vector(log2(SIZE)-1 downto 0)
	);	
end component;

component mmu_tlb_cache is
	generic
	(
		SIZE: natural range 1 to 8 := 4
	);
  port
  (
	  clk : in std_logic;
		reset : in std_logic;
		
		-- From virtual address request to TLB Cache
		din: in tlb_cache_in_type;
		dout: out tlb_cache_out_type;
		
		-- From TLB Cache to Joint TLB
		jout: out tlb_cache_req_type;
		jin: in joint_tlb_mem_in_type
	);	
end component;

component mmu_joint_tlb is
	generic
	(
	  TLB_SIZE: natural range 16 to 64 := 32;
		HASH_SIZE: natural range 64 to 2048 := 1024
	);
  port
  (
	  clk : in std_logic;
		reset : in std_logic;			
		
		-- From TLB caches to Joint TLB
		din: in joint_tlb_in_type;
		dout: out joint_tlb_out_type;
		
		-- From Joint TLB to external memory
		mem_out: out joint_tlb_mem_out_type;
		mem_in: in joint_tlb_mem_in_type
	);	
end component;

component mmu is
	generic
	(
	  ITLB_CACHE_SIZE: natural range 1 to 8 := 4;
		DTLB_CACHE_SIZE: natural range 1 to 8 := 4;
	  JOINT_TLB_SIZE: natural range 16 to 64 := 32;
		JOINT_TLB_HASH_SIZE: natural range 64 to 2048 := 1024		
	);
  port
  (
	  clk : in std_logic;
		reset : in std_logic;
		
		sptbr: in CSR_sptbr_type;
		mstatus: in CSR_mstatus_type;
		debug_mode: in std_logic;		
		access_except: out std_logic;
		
		-- From ICache to MMU
		itlb_in: in tlb_cache_in_type;
		itlb_out: out tlb_cache_out_type;
		
		-- From DCache to MMU
		dtlb_in: in tlb_cache_in_type;
		dtlb_out: out tlb_cache_out_type;		
		
		-- From MMU to Memory
		mem_out: out mmu_mem_out_type;
		mem_in: in mmu_mem_in_type
	);	
end component;

component avalon_master is
	generic
	(
	  ENABLE_MMU: natural range 0 to 1 := 0;
		TIMEOUT: natural range 0 to 255 := 0  -- External Bus Timeout, 0 - no timeout control
	);
  port
  (
	  clk : in std_logic;
		reset : in std_logic;  
		
		icache_in: in icache_bus_out_type;
		icache_out: out icache_bus_in_type;
		dcache_in: in dcache_bus_out_type;
		dcache_out: out dcache_bus_in_type;		
		mmu_out: out mmu_mem_in_type;
	  mmu_in: in mmu_mem_out_type;		
		
		-- Avalon Bus Master Interface
		avm_mem_address: out std_logic_vector(PADDRESS_WIDTH-1 downto 0);
		avm_mem_burstcount: out std_logic_vector(log2(imax(MAX_ICACHE_LINE_SIZE, MAX_DCACHE_LINE_SIZE)) downto 0);
		avm_mem_read: out std_logic;
		avm_mem_readdata: in std_logic_vector(BUS_WIDTH-1 downto 0);
		avm_mem_readdatavalid: in std_logic;
		avm_mem_write: out std_logic;
		avm_mem_writedata: out std_logic_vector(BUS_WIDTH-1 downto 0);
		avm_mem_byteenable: out std_logic_vector(BUS_WIDTH/8 -1 downto 0);
--		avm_mem_lock: out std_logic;
		avm_mem_waitrequest: in std_logic	
	);	
end component;

component ram_avalon is 
	generic
	(									 
	  DEVICE_FAMILY: string := "Cyclone III";
	  RAM_SIZE: natural range 1024 to 65536 := 16384;
		RAM_START_ADDRESS: std_logic_vector(PADDRESS_WIDTH-1 downto 0) := (others => '0');
		RAM_INIT_FILE: string
	);
	port
	(
	  clk : in std_logic;
		reset : in std_logic;  
		icache_in: in icache_fetch_in_type;
		icache_out: out icache_fetch_out_type;
		dcache_in: in dcache_mem_in_type;
		dcache_out: out dcache_mem_out_type;		
		
		-- Avalon Bus Master Interface
		avm_mem_address: out std_logic_vector(PADDRESS_WIDTH-1 downto 0);
		avm_mem_burstcount: out std_logic_vector(log2(imax(MAX_ICACHE_LINE_SIZE, MAX_DCACHE_LINE_SIZE)) downto 0);
		avm_mem_read: out std_logic;
		avm_mem_readdata: in std_logic_vector(BUS_WIDTH-1 downto 0);
		avm_mem_readdatavalid: in std_logic;
		avm_mem_write: out std_logic;
		avm_mem_writedata: out std_logic_vector(BUS_WIDTH-1 downto 0);
		avm_mem_byteenable: out std_logic_vector(BUS_WIDTH/8 -1 downto 0);
		avm_mem_waitrequest: in std_logic			
	);	
end component;

component dbg_vjtag is
	port
	(
		ir_out		: in std_logic_vector(4 downto 0);
		tdo		: in std_logic;
		ir_in		: out std_logic_vector(4 downto 0);
		tck		: out std_logic;
		tdi		: out std_logic;
		virtual_state_cdr		: out std_logic;
		virtual_state_cir		: out std_logic;
		virtual_state_e1dr	: out std_logic;
		virtual_state_e2dr	: out std_logic;
		virtual_state_pdr		: out std_logic;
		virtual_state_sdr		: out std_logic;
		virtual_state_udr		: out std_logic;
		virtual_state_uir		: out std_logic	
	);
end component;

component dbg_crc is
	generic
	(
		LENGTH: natural range 8 to 32 := 8  -- 8, 16 or 32 supported values
	);
	port
	(
	  clk : in std_logic;
		reset : in std_logic;
		data: in std_logic;
		enable: in std_logic;
		shift: in std_logic;
		sclr: in std_logic;
		crc_out: out std_logic_vector(31 downto 0)
	);
end component;

component dbg_jtag_transport is
	generic
	(
	  DATA_WIDTH: natural range 32 to 128 := 32;
		ADDRESS_WIDTH: natural range 32 to 128 := 32;
		SERIAL_WIDTH: natural range 8 to 128 := 32;
		INT_BITS: natural range 1 to 32 := 1;
		SER_BITS: natural range 1 to 8 := 1;
		VERSION: natural range 0 to 15 := 1;
		PART_NUMBER: natural range 0 to 65535 := 1;
		MANUF_ID: natural range 0 to 2047 := 1;
		CRC_LENGTH: natural range 8 to 32 := 8  -- 8, 16 or 32 supported values
	);
  port
  (
	  clk: in std_logic;
		reset: in std_logic;

		dbg_reset: out std_logic;  -- Debugger reset output
		dbg_ndreset: out std_logic;  -- Debugger non-halt reset	mode
		
		-- Interface to Bus Bridge
		bus_tck: out std_logic;		-- JTAG clock
		bus_address: out std_logic_vector(ADDRESS_WIDTH-1 downto 0);
		bus_size: out std_logic_vector(2 downto 0);
		bus_strobe: out std_logic;
		bus_writedata: out std_logic_vector(DATA_WIDTH-1 downto 0);
		bus_write: out std_logic;
		bus_readdata: in std_logic_vector(DATA_WIDTH-1 downto 0);
		bus_ready: in std_logic;
		
    -- Signals from Virtual JTAG instance
    tck: in std_logic;
	  tdi: in std_logic;
    capture_dr: in std_logic;
	  e1dr: in std_logic;
	  e2dr: in std_logic;
	  pdr: in std_logic;
	  shift_dr: in std_logic;
	  update_dr: in std_logic;
	  update_ir: in std_logic;
	  cir: in std_logic;
	  tms: in std_logic;
		IR: in std_logic_vector(4 downto 0); -- IR command register	
    tdo: out std_logic		
	);	
end	component;

component dbg_avalon_bridge is
	generic
	(
	  DATA_WIDTH: natural range 32 to 128 := 32;
		ADDRESS_WIDTH: natural range 32 to 128 := 32
	);
  port
  (
	  clk : in std_logic;   -- Avalon bus clock
		reset : in std_logic;
		
		tck: in std_logic;		-- JTAG clock
		
		bus_address: in std_logic_vector(ADDRESS_WIDTH-1 downto 0);
		bus_size: in std_logic_vector(2 downto 0);
		bus_strobe: in std_logic;
		bus_writedata: in std_logic_vector(DATA_WIDTH-1 downto 0);
		bus_write: in std_logic;
		bus_readdata: out std_logic_vector(DATA_WIDTH-1 downto 0);
		bus_ready: out std_logic;
		
		-- Avalon Master
		avm_dbgm_address: out std_logic_vector(ADDRESS_WIDTH-1 downto 0);
		avm_dbgm_read: out std_logic;
		avm_dbgm_readdata: in std_logic_vector(DATA_WIDTH-1 downto 0);
		avm_dbgm_readdatavalid: in std_logic;
		avm_dbgm_write: out std_logic;
		avm_dbgm_writedata: out std_logic_vector(DATA_WIDTH-1 downto 0);
		avm_dbgm_byteenable: out std_logic_vector(DATA_WIDTH/8-1 downto 0);
		avm_dbgm_waitrequest: in std_logic		
	);	
end	component;

component dbg_module_avalon is
	generic
	(
		ROM_INIT_FILE: string
	);
	port
	(
	  clk: in std_logic;
		reset: in std_logic;
		
		dbg_irq0: out std_logic;
		
		-- Avalon Bus Slave
		avs_s1_address: in std_logic_vector(9 downto 0);
		avs_s1_writedata: in std_logic_vector(31 downto 0); 
		avs_s1_chipselect: in std_logic;
		avs_s1_read: in std_logic;
		avs_s1_write: in std_logic;	
		avs_s1_byteenable: in std_logic_vector(3 downto 0);
		avs_s1_readdata: out std_logic_vector(31 downto 0);
		avs_s1_waitrequest: out std_logic
--		avs_s1_readdatavalid: out std_logic
	);	
end component;

component hw_breakpoints is
	generic
	(
	  INSN_BP_NUM: natural range 1 to 16 := 4;
		DATA_BP_NUM: natural range 1 to 16 := 2;
		ENABLE_FULL_READ: natural range 0 to 1 := 1
	);
  port
  (
	  clk : in std_logic;
		reset : in std_logic;
		
		din: in hw_breakpoints_in_type;
		dout: out hw_breakpoints_out_type;
		
		csr_in: in extern_csr_in_type;
		readdata: out std_logic_vector(WORD_SIZE-1 downto 0)
	);	
end component;

-- Single clock FIFO
component fifo_sc is
	generic
	(
	  WIDTH : natural;
		DEPTH : natural;
		DEVICE_FAMILY : string := ""
	);
	port
	(
	  clk: in std_logic;
	  reset : in std_logic;
		data : in std_logic_vector(WIDTH-1 downto 0);
		rdreq	: in std_logic;
		wrreq	: in std_logic;
		flush : in std_logic;
		empty	: out std_logic;
		full : out std_logic;
		q	: out std_logic_vector(WIDTH-1 downto 0);
		usedw	: out std_logic_vector(ceil_log2(DEPTH)-1 downto 0)
	);	
end component;

component trace_fifo is
	generic
	(
	  TRACE_WIDTH : natural range 32 to 256 := 128;
		FIFO_WIDTH : natural range 32 to 256 := 128; -- must be FIFO_WIDTH >= TRACE_WIDTH
		FIFO_DEPTH : natural := 256;
		OUT_WIDTH : natural range 32 to 128 := 32;	-- must be FIFO_WIDTH/OUT_WIDTH = 2**N	
		DEVICE_FAMILY : string := ""
	);
	port
	(
	  clk: in std_logic;
	  reset : in std_logic;
		
		trace : in std_logic_vector(TRACE_WIDTH-1 downto 0);
		trace_len : in std_logic_vector(ceil_log2(TRACE_WIDTH)-1 downto 0);
		trace_wrreq	: in std_logic;
		
		flush : in std_logic;
		bus_rdreq	: in std_logic;
		
		full : out std_logic;
		q	: out std_logic_vector(OUT_WIDTH-1 downto 0);
		num_word : out std_logic_vector(ceil_log2(FIFO_DEPTH*FIFO_WIDTH/OUT_WIDTH)-1 downto 0)
	);	
end	component;

component trace is
	generic
	(
	  FIFO_SIZE: natural range 128 to 65536 := 2048; -- Number of bytes
		FIFO_WIDTH: natural range 32 to 512 := 128;		 -- Number of bits	
		INIT_BUF_START: integer := 0;
		INIT_BUF_END: integer	:= 0
	);
  port
  (
	  clk: in std_logic;
		reset: in std_logic;
		
		din: in trace_in_type;
		dout: out trace_out_type;
		cmd_in: in trace_cmd_in_type;
		csr_in: in trace_csr_in_type;
    csr_out: out trace_csr_out_type;
		bus_in: in trace_bus_in_type;
    bus_out: out trace_bus_out_type
  );		
end component;

component rtcc is
	generic
	(
		CLK_FREQ: natural := 1   -- input clock frequency in MHz, default parameter for prescaler 
	);
	port
	(
	  clk: in std_logic;
		reset: in std_logic;
		
		intr: out std_logic;
		
		-- Avalon Bus Slave
		avs_s1_address: in std_logic_vector(2 downto 0);
		avs_s1_writedata: in std_logic_vector(31 downto 0); 
		avs_s1_chipselect: in std_logic;
		avs_s1_read: in std_logic;
		avs_s1_write: in std_logic;
		avs_s1_readdata: out std_logic_vector(31 downto 0)		
	);	
end	component;

component plic is
	generic
	(
		IRQ_NUM: natural range 1 to 32 := 8  -- Number of external interrupts
	);
	port
	(
	  clk: in std_logic;
		reset: in std_logic;
		
		irq: in std_logic_vector(IRQ_NUM-1 downto 0);
		intr: out std_logic;
		
		-- Avalon Bus Slave
		avs_s1_address: in std_logic_vector(2 downto 0);
		avs_s1_writedata: in std_logic_vector(31 downto 0); 
		avs_s1_chipselect: in std_logic;
		avs_s1_read: in std_logic;
		avs_s1_write: in std_logic;
		avs_s1_readdata: out std_logic_vector(31 downto 0)		
	);	
end	component;

end package iface;

package body iface is

end package body iface;
