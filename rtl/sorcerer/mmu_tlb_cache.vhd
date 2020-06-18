-- Project Sorcerer - RISC-V core
--
-- Local TLB's, small & fast fully-associated
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
use work.pipeline_iface.all;
use work.iface.all;

entity mmu_tlb_cache is
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
end mmu_tlb_cache;

architecture behaviour of mmu_tlb_cache is
  constant INDEX_BITS: natural := ceil_log2(SIZE);

  type tlb_row_type is record
	  vpn: std_logic_vector(VPN_BITS-1 downto 0);
		asid: std_logic_vector(ASID_BITS-1 downto 0);
		ppn: std_logic_vector(PPN_BITS-1 downto 0);
		d: std_logic;
		a: std_logic;		
		g: std_logic;
		u: std_logic;
		x: std_logic;
		w: std_logic;
		r: std_logic;
		v: std_logic;
	end record;
	type tlb_row_array is array (0 to SIZE-1) of tlb_row_type;

	type state_type is (st_execute, st_wait_jtlb);
	type tlb_regs_type is record
		state: state_type;
		rows: tlb_row_array;
	end record;	

	signal r, next_r: tlb_regs_type;
	
	signal wr_index: std_logic_vector(INDEX_BITS-1 downto 0);
	signal touch: std_logic;
	signal touch_pos: std_logic_vector(INDEX_BITS-1 downto 0);	
begin
  lru1: lru
	generic	map
	(
		SIZE => SIZE
	)
  port map
  (
	  clk => clk,
		reset => reset,
		touch => touch,
		touch_pos => touch_pos,
		last_pos => wr_index
	);

	next_state_proc: process(r, din, jin, wr_index)
	  variable v: tlb_regs_type;
		variable req: std_logic;
		variable paddr: std_logic_vector(PADDRESS_WIDTH-1 downto 0);
		variable hit: std_logic;
		variable hit_index:  std_logic_vector(INDEX_BITS-1 downto 0);
		variable invalid: std_logic;
		variable modified: std_logic;
		
		procedure doMatch(row: tlb_row_type; index: natural) is
		begin
			if (din.vaddress(PAGE_IDX_BITS+VPN_BITS-1 downto PAGE_IDX_BITS) = row.vpn) and
				 ((not isTLBCacheUseASID) or (isTLBCacheUseASID and (row.g = '1' or din.asid = row.asid)))
			then			
			  paddr(PAGE_IDX_BITS+PPN_BITS-1 downto PAGE_IDX_BITS) := 
			    paddr(PAGE_IDX_BITS+PPN_BITS-1 downto PAGE_IDX_BITS) or row.ppn;
				if row.v = '0' then
					invalid := '1';
				end if;	
				if row.d = '0' and din.store = '1' then
					modified := '1';				
				end if;	 
				if row.v = '1' then
				  hit := '1';
				end if;	
				hit_index := hit_index or CONV_STD_LOGIC_VECTOR(index, INDEX_BITS);
			end if;
		end procedure doMatch;
		
		procedure doWrite(row: out tlb_row_type) is
		  variable ppn: std_logic_vector(jin.pte.ppn1'length+jin.pte.ppn0'length-1 downto 0);
		begin
			row.vpn := din.vaddress(PAGE_IDX_BITS+VPN_BITS-1 downto PAGE_IDX_BITS);
			ppn := jin.pte.ppn1 & jin.pte.ppn0;
			row.ppn := ppn(PPN_BITS-1 downto 0);
			row.asid := din.asid;
			row.d := jin.pte.d;
			row.a := jin.pte.a;
			row.g := jin.pte.g;
			row.u := jin.pte.u;
			row.x := jin.pte.x;
			row.w := jin.pte.w;
			row.r := jin.pte.r;
			row.v := jin.pte.v;
		end procedure doWrite;	
		
	begin
		v := r;

		paddr := (others => '0');		
		hit := '0';
		hit_index := (others => '0');
		touch <= '0';
		invalid := '0';
		modified := '0'; 
		req := din.load or din.store or din.execute;
		dout.ready <= '0'; 
		jout.req <= '0';
		
		if din.enableVM = '1' then
		  for i in 0 to SIZE-1 loop
			  doMatch(r.rows(i), i);	
		  end loop;				
			
			touch <= hit and (din.load or din.store);
			
			case r.state is
				when st_execute =>
				  if din.flush = '1' then
						for i in 0 to SIZE-1 loop
							v.rows(i).v := '0';	
						end loop;	
				  elsif req = '1' then
						if hit = '1' then
							dout.ready <= '1';
							touch <= '1';
						else
							jout.req <= '1';
							v.state := st_wait_jtlb;
						end if;	
					end if;
				when st_wait_jtlb =>
				  if jin.ready = '1' then
						doWrite(v.rows(CONV_INTEGER(wr_index)));
						v.state := st_execute;
					else	
						jout.req <= '1';
					end if;
			end case;
		else
			touch <= '0';
			paddr(PAGE_IDX_BITS-1 downto 0) := din.vaddress(PAGE_IDX_BITS-1 downto 0);
		end if;
		touch_pos <= hit_index;		
		
		next_r <= v;
		dout.paddress <= paddr;
		dout.invalid <= invalid;
		dout.modified <= modified;
		dout.miss <= (din.load or din.store or din.execute) and not hit;
	end process next_state_proc;
	
	process(reset, clk)
	begin
		if reset = '1' then
			r.state <= st_execute;
			for i in 0 to SIZE-1 loop
				r.rows(i).v <= '0';
			end loop;	
		elsif rising_edge(clk) then	
			r <= next_r;
		end if;
	end process;

	dout.error <= jin.error;
	jout.vpn1 <= din.vaddress(PAGE_IDX_BITS+10+9 downto PAGE_IDX_BITS+10);
	jout.vpn0 <= din.vaddress(PAGE_IDX_BITS+9 downto PAGE_IDX_BITS);
	jout.asid <= din.asid;
end behaviour;
