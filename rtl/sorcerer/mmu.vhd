-- Project Sorcerer - RISC-V core
--
-- Memory Management Unit (MMU)
-- Consists of small seperate TLB for Instruction and Data (I-TLB & D-TLB)
-- and large common TLB for instruction and data (Joint TLB, J-TLB)
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
use work.csr.all;
use work.pipeline_iface.all;
use work.iface.all;

entity mmu is
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
end mmu;

architecture behaviour of mmu is
  constant PTE_LSB: natural := log2(WORD_SIZE/8);

  function base_address(sptbr: CSR_sptbr_type) return std_logic_vector is
 	  variable addr: std_logic_vector(PADDRESS_WIDTH-1 downto 0);
	begin																								
		addr(PADDRESS_WIDTH-1 downto PAGE_IDX_BITS) := sptbr.ppn(PADDRESS_WIDTH-PAGE_IDX_BITS-1 downto 0);
	  addr(PAGE_IDX_BITS-1 downto 0) := (others => '0');
		return addr;
	end function base_address;
	
	function pte_offset(vpn: std_logic_vector(9 downto 0)) return std_logic_vector is
	  variable addr: std_logic_vector(PADDRESS_WIDTH-1 downto 0);
	begin										 											 
		addr(PADDRESS_WIDTH-1 downto vpn'length+PTE_LSB) := (others => '0');
		addr(vpn'high+PTE_LSB downto PTE_LSB) := vpn;
		addr(PTE_LSB-1 downto 0) := (others => '0');
		return addr;
	end function pte_offset;	
	
	function pte_address(pte: pte_type) return std_logic_vector is
	  variable addr: std_logic_vector(PADDRESS_WIDTH-1 downto 0);
	begin																																 
		addr(PADDRESS_WIDTH-1 downto pte.ppn0'high+PAGE_IDX_BITS+1) := 
		  pte.ppn1(PADDRESS_WIDTH-pte.ppn0'length-PAGE_IDX_BITS-1 downto 0);
		addr(pte.ppn0'high+PAGE_IDX_BITS downto PAGE_IDX_BITS) := pte.ppn0;
		addr(PAGE_IDX_BITS-1 downto 0) := (others => '0');
		return addr;
	end function pte_address;	
	
  type state_type is (st_idle, st_watch_jtlb, st_request_walk_mem1, st_walk_mem1, 
	  st_level2, st_request_walk_mem2, st_walk_mem2, st_commit);
	type mmu_regs_type is record
		state: state_type;
		way: std_logic;  -- '0' - ITLB, '1' - DTLB
		address: std_logic_vector(ADDRESS_WIDTH-1 downto 0);
		pte: pte_type;
	end record;	

	signal r, next_r: mmu_regs_type;

  signal itlb_jout: tlb_cache_req_type;
	signal itlb_jin: joint_tlb_mem_in_type;
	
  signal dtlb_jout: tlb_cache_req_type;
	signal dtlb_jin: joint_tlb_mem_in_type;
	
	signal jtlb_in: joint_tlb_in_type;
	signal jtlb_out: joint_tlb_out_type;
	signal jtlb_mem_out: joint_tlb_mem_out_type;
	signal jtlb_mem_in: joint_tlb_mem_in_type;
	
	signal error: std_logic;
begin
  itlb_cache: mmu_tlb_cache
	generic map
	(
		SIZE => ITLB_CACHE_SIZE
	)
  port map
  (
	  clk => clk,
		reset => reset,
		din => itlb_in,
		dout => itlb_out,
		jout => itlb_jout,
		jin => itlb_jin
	);
	
  dtlb_cache: mmu_tlb_cache
	generic map
	(
		SIZE => DTLB_CACHE_SIZE
	)
  port map
  (
	  clk => clk,
		reset => reset,
		din => dtlb_in,
		dout => dtlb_out,
		jout => dtlb_jout,
		jin => dtlb_jin
	);	

  jtlb: mmu_joint_tlb
	generic	map
	(
	  TLB_SIZE => JOINT_TLB_SIZE,
		HASH_SIZE => JOINT_TLB_HASH_SIZE
	)
  port map
  (
	  clk => clk,
		reset => reset,
		din => jtlb_in,
		dout => jtlb_out,
		mem_out => jtlb_mem_out,
		mem_in => jtlb_mem_in
	);	
	
	next_state_proc: process(r, itlb_jout, dtlb_jout, jtlb_out, jtlb_mem_out, mem_in, sptbr)
	  variable v: mmu_regs_type;
		variable way: std_logic;
		
		procedure walk_mem(read: std_logic; next_state: state_type; wait_state: state_type) is
		begin
			mem_out.read <= read;	
			if mem_in.readdatavalid = '1' then
				v.pte := pte_conv(mem_in.readdata);
				v.state := next_state;	
			elsif read = '1' and mem_in.waitrequest = '0' then
				v.state := wait_state;
			end if;									
		  if mem_in.timeout = '1' then
				v.state := st_idle;
				error <= '1';
			end if;			
		end procedure walk_mem;	
	begin
	  v := r;		 
		error <= '0';
		mem_out.read <= '0'; 
		jtlb_in.req <= '0';
		jtlb_mem_in.ready <= '0';
		access_except <= '0';
		way := r.way;
		case r.state is
			when st_idle =>
			  way := dtlb_jout.req;	
				if itlb_jout.req = '1' or dtlb_jout.req = '1' then
					jtlb_in.req <= '1';
					v.state := st_watch_jtlb;
				end if;	
			when st_watch_jtlb => 						 
			  if jtlb_out.ready = '1' then
					v.state := st_idle;
				elsif jtlb_mem_out.req = '1' then
					v.address := base_address(sptbr) + pte_offset(jtlb_mem_out.vpn1);
					v.state := st_request_walk_mem1;
				end if;
			when st_request_walk_mem1 =>
			  walk_mem('1', st_level2, st_walk_mem1);
			when st_walk_mem1 =>
				walk_mem('0', st_level2, st_walk_mem1);
			when st_level2 =>	
			  v.address := pte_address(r.pte) + pte_offset(jtlb_mem_out.vpn0);
			  if r.pte.v = '0' or (r.pte.r = '0' and r.pte.w = '1')	 then
					access_except <= '1';
					v.state := st_idle; -- TODO (special state?)
				else
				  v.state := st_request_walk_mem2;
				end if;
			when st_request_walk_mem2 =>
			  walk_mem('1', st_commit, st_walk_mem2);
			when st_walk_mem2 =>
				walk_mem('0', st_commit, st_walk_mem2);				
			when st_commit =>				 
				if r.pte.r = '1' or r.pte.x = '1' then  -- leaf PTE
				  jtlb_mem_in.ready <= '1';											 
			    v.state := st_idle;
				else
					access_except <= '1';
					v.state := st_idle; -- TODO (special state?)					
				end if;	
		end case;
		v.way := way;
		if way = '1' then
			jtlb_in.vpn0 <= dtlb_jout.vpn0;
			jtlb_in.vpn1 <= dtlb_jout.vpn1;
		else	
			jtlb_in.vpn0 <= itlb_jout.vpn0;
			jtlb_in.vpn1 <= itlb_jout.vpn1;			
		end if;		
		next_r <= v;
	end process next_state_proc;	
	
	process(reset, clk)
	begin
		if reset = '1' then
			r.state <= st_idle;
			r.way <= '0';
		elsif rising_edge(clk) then	
			r <= next_r;
		end if;
	end process;
	
	itlb_jin.pte <= jtlb_out.pte;
	itlb_jin.ready <= jtlb_out.ready and not r.way;	
	itlb_jin.error <= error and not r.way;	
	dtlb_jin.pte <= jtlb_out.pte;
	dtlb_jin.ready <= jtlb_out.ready and r.way;
	dtlb_jin.error <= error and r.way;
	
	jtlb_in.flush <= itlb_in.flush or dtlb_in.flush;
	jtlb_mem_in.pte <= r.pte;
	
	mem_out.address <= r.address;	
end behaviour;
