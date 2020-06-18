-- Project Sorcerer - RISC-V core
--
-- Instruction Cache
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
use work.memory_map.all;
use work.pipeline_iface.all;
use work.iface.all;
use work.cache_utils.all;

entity icache is 
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
end icache;

architecture behaviour of icache is	 
  constant DATA_WIDTH: natural := ICACHE_FETCH_WIDTH;
	constant DATA_BITS: natural := log2(DATA_WIDTH/8);
	constant LINE_BITS: natural := log2(LINE_SIZE);
  constant COL_ADDRESS_BITS: natural := log2(SIZE/ASSOCIATIVITY) - DATA_BITS;
	constant TAG_WIDTH : natural := PADDRESS_WIDTH - log2(SIZE/ASSOCIATIVITY);
	constant TAG_ADDRESS_BITS: natural := log2(SIZE/ASSOCIATIVITY) - LINE_BITS - DATA_BITS;
	constant LRU_BITS: natural := lru_width(ASSOCIATIVITY);
	
	subtype column_line_type is std_logic_vector(DATA_WIDTH-1 downto 0);
	type column_array is array (0 to 2**COL_ADDRESS_BITS-1) of column_line_type;
	type column_ram_type is array(0 to ASSOCIATIVITY-1) of column_array;
	type column_data_array is array (0 to ASSOCIATIVITY-1) of column_line_type;
	
	type tag_type is record
		valid: std_logic;
		tag: std_logic_vector(TAG_WIDTH-1 downto 0);
	end record;
	type tag_array is array(0 to ASSOCIATIVITY-1) of tag_type;
	type tag_line_type is record
		tags: tag_array;
		lru: std_logic_vector(LRU_BITS-1 downto 0);
	end record;	
	
	constant TAG_LINE_WIDTH: natural := LRU_BITS + ASSOCIATIVITY*(TAG_WIDTH+1);
	
	function tag_line_to_vector(line: tag_line_type) return std_logic_vector is
	  variable vec: std_logic_vector(TAG_LINE_WIDTH-1 downto 0);
		variable offset: natural;
	begin											 
		offset := 0;
		for i in 0 to ASSOCIATIVITY-1 loop
			vec(offset+TAG_WIDTH-1 downto offset) := line.tags(i).tag;
			vec(offset+TAG_WIDTH) := line.tags(i).valid;
			offset := offset + TAG_WIDTH + 1;
		end loop;
		vec(TAG_LINE_WIDTH-1 downto TAG_LINE_WIDTH-LRU_BITS) := line.lru;
		return vec;
	end function tag_line_to_vector;	

	function vector_to_tag_line(vec: std_logic_vector(TAG_LINE_WIDTH-1 downto 0)) return tag_line_type is
	  variable line: tag_line_type;
		variable offset: natural;
	begin
		offset := 0; 
		for i in 0 to ASSOCIATIVITY-1 loop
			line.tags(i).tag := vec(offset+TAG_WIDTH-1 downto offset);
			line.tags(i).valid := vec(offset+TAG_WIDTH);
			offset := offset + TAG_WIDTH + 1;
		end loop;		
		line.lru := vec(TAG_LINE_WIDTH-1 downto TAG_LINE_WIDTH-LRU_BITS);
		return line;
	end function vector_to_tag_line; 	
	
	type tag_ram_type is array (0 to 2**TAG_ADDRESS_BITS-1) of tag_line_type;

	type column_cmd_type is record
	  rd_address: std_logic_vector(COL_ADDRESS_BITS-1 downto 0);
		wr_address: std_logic_vector(COL_ADDRESS_BITS-1 downto 0);
	  wr: std_logic_vector(ASSOCIATIVITY-1 downto 0);
	  wr_data: std_logic_vector(DATA_WIDTH-1 downto 0);		
	end record;	
	
	type tag_cmd_type is record
	  rd_address: std_logic_vector(TAG_ADDRESS_BITS-1 downto 0);
		wr_address: std_logic_vector(TAG_ADDRESS_BITS-1 downto 0);
	  wr: std_logic;
	  wr_data: tag_line_type;		
	end record;		

  type state_type is (st_clear_tags, st_execute,
	  st_request_read_bypass, st_read_bypass, st_request_read, st_read_data,
	  st_commit);
	type icache_regs_type is record
		paddress: std_logic_vector(PADDRESS_WIDTH-1 downto 0);
		state: state_type;
		cmd: std_logic;
		counter: std_logic_vector(TAG_ADDRESS_BITS-1 downto 0);
		victim_sel: std_logic_vector(ASSOCIATIVITY-1 downto 0);
		jump: std_logic; 
		flush: std_logic;
	end record;	
	
	signal next_pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
	
  signal column_cmd: column_cmd_type;	
	type column_rd_data_type is array (0 to ASSOCIATIVITY-1) of std_logic_vector(DATA_WIDTH-1 downto 0);
	signal column_rd_data: column_rd_data_type;	
	signal tag_cmd: tag_cmd_type;	
	signal tag_rd_data: tag_line_type;	
	
	signal column_ram: column_ram_type;	 
	type column_rd_address_type is array (0 to ASSOCIATIVITY-1) of std_logic_vector(COL_ADDRESS_BITS-1 downto 0);
	signal column_address_reg: column_rd_address_type;
	signal tag_ram: tag_ram_type;
	signal tag_address_reg: std_logic_vector(TAG_ADDRESS_BITS-1 downto 0);
	
	signal r, next_r: icache_regs_type;
begin		
	next_pc <= din.next_pc when r.jump = '0' else din.pc;
	
	colume_sim: if SYNTH = 0 generate
		column_proc: for i in 0 to ASSOCIATIVITY-1 generate
			column_ram_proc: process(clk)
			begin
				if rising_edge(clk) then
					if column_cmd.wr(i) = '1' then
						column_ram(i)(CONV_INTEGER(column_cmd.wr_address)) <= column_cmd.wr_data;
					end if;
					column_address_reg(i) <= column_cmd.rd_address;
				end if;	
			end process column_ram_proc;
		  column_rd_data(i) <= column_ram(i)(CONV_INTEGER(column_address_reg(i)));
		end generate;	
	end generate;

	column_syn: if SYNTH = 1 generate
		column: for i in 0 to ASSOCIATIVITY-1 generate
      column_ram: ram_block
	    generic map
	    (
	      SIZE => SIZE/ASSOCIATIVITY,
		    DATA_WIDTH => DATA_WIDTH
	    )
      port map
      (
        clk => clk,
	      rdaddress => column_cmd.rd_address,
	      wraddress => column_cmd.wr_address,
	      data => column_cmd.wr_data,
	      wren => column_cmd.wr(i),
	      byteen => (others => '1'),
	      q => column_rd_data(i)
      );			
		end generate;	
	end generate;		
	
	tag_ram_proc: process(clk)
	begin
		if rising_edge(clk) then
			if tag_cmd.wr = '1' then
				tag_ram(CONV_INTEGER(tag_cmd.wr_address)) <= tag_cmd.wr_data;
			end if;
			tag_address_reg <= tag_cmd.rd_address;
		end if;
	end process tag_ram_proc;
	tag_rd_data <= tag_ram(CONV_INTEGER(tag_address_reg));
	
	next_state_proc: process(r, din, mmu_in, bus_in, next_pc, tag_rd_data, column_rd_data, cmd_in)
	  variable v: icache_regs_type;
		variable fix_map_address: std_logic_vector(PADDRESS_WIDTH-1 downto 0);
		variable paddress: std_logic_vector(PADDRESS_WIDTH-1 downto 0);
		variable hit: std_logic_vector(ASSOCIATIVITY-1 downto 0);	
		variable col_data: column_line_type;
		variable rd_lru: std_logic_vector(LRU_BITS-1 downto 0);
		variable next_lru: std_logic_vector(LRU_BITS-1 downto 0);
		
		procedure readWord is
		begin
			if ASSOCIATIVITY = 1 then
				column_cmd.wr <= (others => '1');
			else
				column_cmd.wr <= r.victim_sel;
			end if;
			if r.counter = LINE_SIZE-1 then
				tag_cmd.wr <= '1';
			  v.state := st_commit;	
			end if;	
		  v.counter := r.counter + 1;			
		end procedure readWord;	
		
		procedure check_timeout is
		begin
		  if bus_in.timeout = '1' then
				v.state := st_execute;
				dout.error <= '1';
			end if;	
		end procedure check_timeout;	
	begin
		v := r;	
		column_cmd.wr <= (others => '0');	
		column_cmd.rd_address <= (others => '0');
		column_cmd.wr_address <= (others => '0');
		column_cmd.wr_data <= bus_in.readdata;		
	  tag_cmd.rd_address <= (others => '0');
		tag_cmd.wr_address <= (others => '0');
	  tag_cmd.wr <= '0';
		tag_cmd.wr_data <= tag_rd_data;
		
		fix_map_address := mapVAddress(din.pc);
--		if ENABLE_MMU = 1 then															 
--			bus_out.address(bus_out.address'high downto PAGE_IDX_BITS) <= r.paddress(bus_out.address'high downto PAGE_IDX_BITS);
--			bus_out.address(PAGE_IDX_BITS-1 downto DATA_BITS) <= din.pc(PAGE_IDX_BITS-1 downto DATA_BITS);
--			bus_out.address(DATA_BITS-1 downto 0) <= (others => '0');	
--		else	
--		  bus_out.address(bus_out.address'high downto DATA_BITS) <= fix_map_address(bus_out.address'high downto DATA_BITS);
--			bus_out.address(DATA_BITS-1 downto 0) <= (others => '0');			
--		end if;	
		bus_out.address <= r.paddress;
		bus_out.burstcount <= (others => '0');
		bus_out.read <= '0'; 
		
		mmu_out.fetch <= '0';
		if ENABLE_MMU = 1 and din.vm_en = '1' then
			paddress := mmu_in.paddress;
			dout.error <= mmu_in.error;
		else	
			paddress := fix_map_address;
			dout.error <= '0';
		end if;		
		hit := (others => '0');
		col_data := (others => '0');
		for i in 0 to ASSOCIATIVITY-1 loop
			if (tag_rd_data.tags(i).valid = '1') and 
				-- (din.pc(VADDRESS_WIDTH-1 downto VADDRESS_WIDTH-TAG_WIDTH) = tag_rd_data.tags(i).tag) 
				(paddress(VADDRESS_WIDTH-1 downto VADDRESS_WIDTH-TAG_WIDTH) = tag_rd_data.tags(i).tag) 
			then
				hit(i) := '1';
				col_data := col_data or column_rd_data(i);
			end if;
		end loop;	
		dout.ready <= '0';
		dout.busy <= din.fetch;		
		dout.data <= col_data;
		
	  rd_lru := tag_rd_data.lru;
		v.flush := cmd_in.flush;
		
		case r.state is
			when st_clear_tags =>
				for i in 0 to ASSOCIATIVITY-1 loop
					tag_cmd.wr_data.tags(i).valid <= '0';
				end loop;
				tag_cmd.wr_data.lru <= (others => '0');
				tag_cmd.wr <= '1';
		    tag_cmd.wr_address <=	r.counter;		
			  if r.counter = 2**TAG_ADDRESS_BITS-1 then
					v.state := st_execute;
				end if;
				v.counter := r.counter + 1;			
			when st_execute =>
				column_cmd.rd_address <= next_pc(COL_ADDRESS_BITS+DATA_BITS-1 downto DATA_BITS); 
				tag_cmd.rd_address <= next_pc(TAG_ADDRESS_BITS+LINE_BITS+DATA_BITS-1 downto LINE_BITS+DATA_BITS);
				if r.flush = '1' then
					v.flush := '0';
					v.state := st_clear_tags;
			  elsif din.fetch = '1' and din.jump = '0' then
					v.paddress := paddress;
          lru_next(rd_lru, hit, ASSOCIATIVITY, next_lru);
					tag_cmd.wr_data.lru <= next_lru;	
					if (ENABLE_MMU = 1) then
					  mmu_out.fetch <= din.vm_en;
					end if;					
					-- if isCacheable(din.pc) = '1' then 
					if isCacheable(paddress) = '1' then	
						if ((ENABLE_MMU = 1) and (din.vm_en = '0' or mmu_in.ready = '1')) or (ENABLE_MMU = 0) then
							if hit = 0 then 
								column_cmd.rd_address <= din.pc(COL_ADDRESS_BITS+DATA_BITS-1 downto LINE_BITS+DATA_BITS) & 
								  CONV_STD_LOGIC_VECTOR(0, LINE_BITS);
								v.victim_sel := (others => '0');
								v.victim_sel(lru_victim(rd_lru, ASSOCIATIVITY)) := '1';
								v.counter := (others => '0');
								v.state := st_request_read;
							else
								tag_cmd.wr <= '1';
								tag_cmd.wr_address <= din.pc(TAG_ADDRESS_BITS+LINE_BITS+DATA_BITS-1 downto LINE_BITS+DATA_BITS);
								dout.busy <= '0';
								dout.ready <= '1';
							end if;
						end if;
					else
						if din.fetch = '1' then
							v.state := st_request_read_bypass;
						end if;	
					end if;
				else
					dout.busy <= '0';
				end if;			
	    when st_request_read_bypass =>				 
			  bus_out.read <= '1';
			  dout.data <= bus_in.readdata;
				bus_out.burstcount <= CONV_STD_LOGIC_VECTOR(1, bus_out.burstcount'length);
			  if bus_in.readdatavalid = '1' then
					dout.ready <= '1';
					dout.busy <= '0';
					v.state := st_execute;
				elsif bus_in.waitrequest = '0' then
					v.state := st_read_bypass;
				else	
					dout.busy <= '1';
				end if;
				check_timeout;
			when st_read_bypass =>
			  dout.data <= bus_in.readdata;
			  if bus_in.readdatavalid = '1' then
					dout.ready <= '1';
					dout.busy <= '0';
					v.state := st_execute;
				else	
					dout.busy <= '1';
				end if;				
			when st_request_read =>
			  bus_out.address(LINE_BITS-1 downto DATA_BITS) <= (others => '0');
			  bus_out.read <= '1';
				bus_out.burstcount <= CONV_STD_LOGIC_VECTOR(LINE_SIZE, bus_out.burstcount'length);
				column_cmd.wr_address <= r.paddress(COL_ADDRESS_BITS+DATA_BITS-1 downto LINE_BITS+DATA_BITS) & 
				  r.counter(LINE_BITS-1 downto 0);
			  if bus_in.waitrequest = '0' then
					v.state := st_read_data;
				end if;
				if bus_in.readdatavalid = '1' then
					readWord;
				end if;
				bus_out.address(LINE_BITS+DATA_BITS-1 downto DATA_BITS) <= (others => '0');
				check_timeout;
			when st_read_data =>
				column_cmd.wr_address <= r.paddress(COL_ADDRESS_BITS+DATA_BITS-1 downto LINE_BITS+DATA_BITS) & 
				  r.counter(LINE_BITS-1 downto 0); 
				tag_cmd.rd_address <= r.paddress(TAG_ADDRESS_BITS+LINE_BITS+DATA_BITS-1 downto LINE_BITS+DATA_BITS);
        tag_cmd.wr_address <= r.paddress(TAG_ADDRESS_BITS+LINE_BITS+DATA_BITS-1 downto LINE_BITS+DATA_BITS);				
        for i in 0 to ASSOCIATIVITY-1 loop
					if ASSOCIATIVITY = 1 or (ASSOCIATIVITY /= 1 and r.victim_sel(i) = '1') then
					  tag_cmd.wr_data.tags(i).valid <= '1';
						tag_cmd.wr_data.tags(i).tag <= r.paddress(PADDRESS_WIDTH-1 downto PADDRESS_WIDTH-TAG_WIDTH);
					end if;
        end loop;				
				if bus_in.readdatavalid = '1' then
					readWord;
				end if;
				bus_out.address(LINE_BITS+DATA_BITS-1 downto DATA_BITS) <= (others => '0');
				check_timeout;
	    when st_commit =>		 
			  column_cmd.rd_address <= next_pc(COL_ADDRESS_BITS+DATA_BITS-1 downto DATA_BITS);
				tag_cmd.rd_address <= next_pc(TAG_ADDRESS_BITS+LINE_BITS+DATA_BITS-1 downto LINE_BITS+DATA_BITS);
				v.cmd := '0';
			  v.state := st_execute;			
		end case;
		if din.jump = '1' and r.jump = '0' and v.state /= st_execute then
			v.jump := '1';
		end if;	
		if r.jump = '1' and r.state = st_execute then
			v.jump := '0';
		end if;	
		if din.jump = '1' or r.jump = '1' then
			dout.ready <= '0';
		end if;		
		next_r <= v;
	end process next_state_proc;	
	
  process(reset, clk)
	begin
		if reset = '1' then	 
		  r.state <= st_clear_tags;
			r.counter <= (others => '0');
			r.victim_sel <= (others => '0');
      r.cmd <= '0';
			r.jump <= '0';
			r.flush <= '0';
		elsif rising_edge(clk) then	
			r <= next_r;
		end if;	
	end process;	

	mmu_out.vaddress <= din.pc;	
end behaviour;
