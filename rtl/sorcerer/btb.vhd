-- Project Sorcerer - RISC-V core
--
-- Branch Tagret Buffer (BTB)
-- Based on multiple-associative target address cache table
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
use work.pipeline_iface.all;

entity btb is
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
end btb;

architecture behaviour of btb is
  constant TAG_WIDTH: natural := VADDRESS_WIDTH-log2(BTB_SIZE/BTB_ASSOCIATIVITY)-log2(ICACHE_FETCH_WIDTH/8);
	constant DATA_BITS: natural := log2(ICACHE_FETCH_WIDTH/8);
	constant LRU_BITS: natural := lru_width(BTB_ASSOCIATIVITY);
	constant RAM_ADDR_BITS: natural := log2(BTB_SIZE/BTB_ASSOCIATIVITY);

	function get_tag(addr: std_logic_vector(VADDRESS_WIDTH-1 downto 0)) return std_logic_vector is
	begin
		return addr(VADDRESS_WIDTH-1 downto VADDRESS_WIDTH-TAG_WIDTH);
	end function get_tag;	
	
  type btb_entry_type is record
		tag: std_logic_vector(TAG_WIDTH-1 downto 0);
		valid: std_logic;
		target_pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		branch_type: btb_branch_type;
	end record;

	constant btb_entry_init: btb_entry_type :=
	(
	  tag => (others => '0'),
		valid => '0',
		target_pc => (others => '0'),
		branch_type => BT_EMPTY
	);
	
	type column_ram_type is array (0 to BTB_SIZE/BTB_ASSOCIATIVITY-1) of btb_entry_type;
	type column_ram_array is array(0 to BTB_ASSOCIATIVITY-1) of column_ram_type;
	signal btb_columns_ram: column_ram_array := (others => (others => btb_entry_init));	
	
	type shared_entry_type is record
		lru: std_logic_vector(LRU_BITS-1 downto 0);
	end record;																																					 
	constant shared_entry_init: shared_entry_type :=
	(
		lru => (others => '0')
	);
	
	type shared_ram_type is array(0 to BTB_SIZE/BTB_ASSOCIATIVITY-1) of shared_entry_type;
	signal shared_ram: shared_ram_type := (others => shared_entry_init);
	
	signal column_rd_address: std_logic_vector(RAM_ADDR_BITS-1 downto 0);
	signal column_wr: std_logic_vector(BTB_ASSOCIATIVITY-1 downto 0);
	signal column_wr_data: btb_entry_type;
	signal column_wr_address: std_logic_vector(RAM_ADDR_BITS-1 downto 0);
	type column_data_array is array (0 to BTB_ASSOCIATIVITY-1) of btb_entry_type;
	signal column_rd_data: column_data_array;
	
	signal shared_wr: std_logic;
	signal shared_wr_data: shared_entry_type;
	signal shared_rd_data:	shared_entry_type;
	
begin
	columns: for i in 0 to BTB_ASSOCIATIVITY generate
	column_ram_proc: process(clk)
	begin
		if rising_edge(clk) then
			if column_wr(1) = '1' then
				btb_columns_ram(i)(CONV_INTEGER(column_wr_address)) <= column_wr_data;
			end if;
			column_rd_data(i) <= btb_columns_ram(i)(CONV_INTEGER(column_rd_address));
		end if;
	end process column_ram_proc;		
	end generate;	

	shared1: if BTB_ASSOCIATIVITY > 1 generate
	shared_ram_proc: process(clk)
	begin
		if rising_edge(clk) then
			if shared_wr = '1' then
				shared_ram(CONV_INTEGER(column_wr_address)) <= shared_wr_data;
			end if;
			shared_rd_data <= shared_ram(CONV_INTEGER(column_rd_address));
		end if;
	end process shared_ram_proc;		
	end generate;	

	main_proc: process(din, upd_in, column_rd_data, shared_rd_data)
	  variable hit: std_logic_vector(BTB_ASSOCIATIVITY-1 downto 0);
		variable col_target_pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		variable col_branch: btb_branch_type;
	begin
		hit := (others => '0');
		col_target_pc := (others => '0');
		col_branch := (others => '0');
		for i in 0 to BTB_ASSOCIATIVITY-1 loop
			if (column_rd_data(i).valid = '1') and 
				 (get_tag(din.pc) = column_rd_data(i).tag) 
			then
				hit(i) := '1';
				col_target_pc := col_target_pc or column_rd_data(i).target_pc;
				col_branch := col_branch or	column_rd_data(i).branch_type;
			end if;
		end loop;			
		
		if upd_in.update = '1' then	 
			if BTB_ASSOCIATIVITY > 1 then
				if upd_in.btb_set.hit_vec /= 0 then
					column_wr <= upd_in.btb_set.hit_vec;
				else	
					column_wr <= decode(CONV_STD_LOGIC_VECTOR(lru_victim(upd_in.btb_set.lru, BTB_ASSOCIATIVITY), log2(BTB_ASSOCIATIVITY)));
				end if;
			else
				column_wr <= (others => '1');
			end if;	
		end if;	
		shared_wr <= upd_in.update;
		
		dout.btb_set.hit_vec <= hit;
		if hit /= 0 then
		  dout.hit <= '1';
		else
			dout.hit <= '0';
		end if;	
	end process;	
	
	column_rd_address <= din.next_pc(RAM_ADDR_BITS+DATA_BITS-1 downto DATA_BITS);
	column_wr_address <= upd_in.pc(RAM_ADDR_BITS+DATA_BITS-1 downto DATA_BITS);
	column_wr_data.tag <= get_tag(upd_in.pc);
	column_wr_data.target_pc <= upd_in.target_pc;
	column_wr_data.valid <= '1';
	column_wr_data.branch_type <= upd_in.branch_type;
	
	dout.btb_set.lru <= shared_rd_data.lru;
end behaviour;
