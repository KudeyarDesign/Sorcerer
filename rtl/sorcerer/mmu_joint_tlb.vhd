-- Project Sorcerer - RISC-V core
--
-- Joint TLB (J-TLB)
-- implemented as pseudo-associated cache table
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

entity mmu_joint_tlb is
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
end mmu_joint_tlb;

architecture behaviour of mmu_joint_tlb is						
  constant TLB_BITS: natural := log2(TLB_SIZE);
  constant HASH_BITS: natural := log2(HASH_SIZE);

	type key_type is record
		vpn1: std_logic_vector(9 downto 0);
		vpn0: std_logic_vector(9 downto 0);
		asid: std_logic_vector(ASID_BITS-1 downto 0);		
	end record;	

	function hash(key: key_type) return std_logic_vector is
	  variable vkey: std_logic_vector(10*2+ASID_BITS-1 downto 0);
		variable v: std_logic_vector(HASH_BITS-1 downto 0);	
	begin
		vkey := key.asid & key.vpn1 & key.vpn0;
		v := vkey(HASH_BITS-1 downto 0) xor vkey(2* HASH_BITS-1 downto HASH_BITS);
		return v;
	end function hash;	
	
  type value_type is record
		vpn1: std_logic_vector(9 downto 0);
		vpn0: std_logic_vector(9 downto 0);
		asid: std_logic_vector(ASID_BITS-1 downto 0);	  
	  pte: pte_type;
	end record;
	type hash_ram_array is array(0 to HASH_SIZE-1) of std_logic_vector(TLB_BITS-1 downto 0);
	type value_ram_array is array(0 to TLB_SIZE-1) of value_type;

  type state_type is (st_idle, st_hash, st_value, st_wait, st_commit);	
	type jtlb_regs_type is record		
		state: state_type;
		valid: std_logic_vector(TLB_SIZE-1 downto 0);
		value_address: std_logic_vector(TLB_BITS-1 downto 0);
	end record;	
	
	signal r, next_r: jtlb_regs_type;	
	
	signal hash_ram: hash_ram_array;
	signal value_ram: value_ram_array;
	
	signal lru_touch: std_logic;
	signal lru_touch_pos: std_logic_vector(TLB_BITS-1 downto 0);
	signal lru_last_pos: std_logic_vector(TLB_BITS-1 downto 0);

	signal key: key_type;
	signal hash_wr: std_logic;
	signal hash_address: std_logic_vector(HASH_BITS-1 downto 0);
	signal hash_data: std_logic_vector(TLB_BITS-1 downto 0);
	signal hash_q: std_logic_vector(TLB_BITS-1 downto 0);
	
	signal value_wr: std_logic;
	signal value_address: std_logic_vector(TLB_BITS-1 downto 0);
	signal value_q: value_type;
begin
	key.vpn1 <= din.vpn1;
	key.vpn0 <= din.vpn0;
	key.asid <= din.asid;
	hash_address <= hash(key);
	hash_data <= lru_last_pos;
	lru_touch_pos <= r.value_address;
	
  lru1: pseudo_lru
	generic	map
	(
		SIZE => TLB_SIZE
	)
  port map
  (
	  clk => clk,
		reset => reset,
		touch => lru_touch, 
		touch_pos => lru_touch_pos,
		last_pos => lru_last_pos
	);	
	
	next_state_proc: process(r, din, mem_in, hash_address, value_q)
	  variable v: jtlb_regs_type;
	
		function isMatch(din: joint_tlb_in_type; value_q: value_type) return std_logic is
		begin
			if (din.vpn0 = value_q.vpn0) and (din.vpn1 = value_q.vpn1) and
				 ((not isTLBCacheUseASID) or (isTLBCacheUseASID and (value_q.pte.g = '1' or din.asid = value_q.asid)))
			then			
			  return '1';
			else
				return '0';
			end if;
		end function isMatch;	
	
	begin
		v := r;
		v.value_address := hash_q;
		dout.ready <= '0'; 
		mem_out.req <= '0';
		hash_wr <= '0';
		value_wr <= '0';
		lru_touch <= '0';
		case r.state is
			when st_idle =>	
			  if din.flush = '1' then
					v.valid := (others => '0');
			  elsif din.req = '1' then
					v.state := st_hash;
				end if;
			when st_hash =>			
			  v.state := st_value;
			when st_value =>
			  if r.valid(CONV_INTEGER(r.value_address)) = '1' and isMatch(din, value_q) = '1' then
					dout.ready <= '1'; 
					lru_touch <= '1';
					v.state := st_idle;
				else	
					v.state := st_wait;
				end if;
			when st_wait =>
			  mem_out.req <= '1';
				if mem_in.ready = '1' then
					v.value_address := lru_last_pos;
					hash_wr <= '1';
					value_wr <= '1';
					v.state := st_commit;
				end if;
			when st_commit =>					
			  dout.ready <= '1';
			  v.valid(CONV_INTEGER(r.value_address)) := '1';
				v.state := st_value;			
		end case;	
		next_r <= v; 
		dout.pte <= value_q.pte;
	end process next_state_proc;	
	
	hash_ram_proc: process(clk)
	begin
		if rising_edge(clk) then
			if hash_wr = '1' then
				hash_ram(CONV_INTEGER(hash_address)) <= hash_data;	
			end if;	
			hash_q <= hash_ram(CONV_INTEGER(hash_address));
		end if;
	end process hash_ram_proc;	
	
	value_ram_proc: process(clk)
	begin
	  if rising_edge(clk) then
			if value_wr = '1' then		 
				value_ram(CONV_INTEGER(value_address)) <= 
				  (vpn1 => din.vpn1, vpn0 => din.vpn0, asid => din.asid, pte => mem_in.pte);
			end if;	
			value_q <= value_ram(CONV_INTEGER(value_address));
		end if;
	end process value_ram_proc;	
	
	process(reset, clk)
	begin
		if reset = '1' then
			r.state <= st_idle;
			for i in 0 to TLB_SIZE-1 loop
				r.valid(i) <= '0';				
			end loop;			
		elsif rising_edge(clk) then	
			r <= next_r;
		end if;
	end process;
	
	mem_out.vpn0 <= din.vpn0;
	mem_out.vpn1 <= din.vpn1;
	mem_out.asid <= din.asid;
end behaviour;
