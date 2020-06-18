-- Project Sorcerer - RISC-V core
--
-- Bus Master for Altera Avalon MM bus
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

entity avalon_master is
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
end avalon_master;

architecture behaviour of avalon_master is
  type state_type is (st_arbiter, st_read, st_write);	
	type sel_type is (sel_insn, sel_data, sel_mmu);
  type avm_regs_type is record
		state: state_type;
	  sel: sel_type;	
		icache_pending: std_logic; 
		icache_burstcount: std_logic_vector(avm_mem_burstcount'high downto 0);
		counter: std_logic_vector(avm_mem_burstcount'high downto 0);
		timeout_cnt: std_logic_vector(7 downto 0);
	end record;
	
	signal r, next_r: avm_regs_type;
begin															
	next_state_proc: process(r, icache_in, dcache_in, mmu_in,
	    avm_mem_waitrequest, avm_mem_readdatavalid)
		variable v: avm_regs_type;
		variable sel: sel_type; 
		variable icache_burstcount: std_logic_vector(avm_mem_burstcount'high downto 0);
		variable vtimeout: std_logic;
		
		procedure check_timeout is
		begin
			if TIMEOUT > 0 then
				if v.timeout_cnt = TIMEOUT-1 then
					vtimeout := '1';
					v.state := st_arbiter;
				else	
					v.timeout_cnt := r.timeout_cnt + 1;
				end if;	
			end if;	
		end procedure check_timeout;	
	begin
		v := r;
		vtimeout := '0';
		
		if r.icache_pending = '1' then
			icache_burstcount := r.icache_burstcount;
		else
			icache_burstcount := icache_in.burstcount;
		end if;		
		
		sel := sel_insn;
		case r.state is
			when st_arbiter =>	
			  if ENABLE_MMU = 1 and mmu_in.read = '1' then
					sel := sel_mmu;
					v.counter := CONV_STD_LOGIC_VECTOR(1, v.counter'length);
					v.state := st_read;
			  elsif dcache_in.read = '1' or dcache_in.write = '1' then
					sel := sel_data;
					if (dcache_in.write = '1' and avm_mem_waitrequest = '0') or
						 (dcache_in.read = '1' and avm_mem_readdatavalid = '1') then
						v.counter := dcache_in.burstcount - 1;
					else	
					  v.counter := dcache_in.burstcount;
					end if;	
					if v.counter /= 0 then
						if dcache_in.write = '1' then
						  v.state := st_write;	
						else	
							v.state := st_read;
						end if;
					end if;	
				elsif icache_in.read = '1' or r.icache_pending = '1' then 
					v.icache_pending := '0';
					sel := sel_insn;
					v.sel := sel_insn;						 
					if (icache_in.read = '1' and avm_mem_readdatavalid = '1') then
						v.counter := icache_burstcount - 1;
					else	
						v.counter := icache_burstcount;
					end if;	
					if v.counter /= 0 then
						v.state := st_read;
					end if;					
				end if;
				v.timeout_cnt := (others => '0');
			when st_read =>
			  sel := r.sel;
				if avm_mem_readdatavalid = '1' then
					v.counter := r.counter - 1;
					if v.counter = 0 then
						v.state := st_arbiter;
					end if;
				else
					check_timeout;
				end if;	
			when st_write =>
			  sel := r.sel;
			  if avm_mem_waitrequest = '0' then
					v.counter := r.counter - 1;
					if v.counter = 0 then
						v.state := st_arbiter;
					end if;		
				else
					check_timeout;
				end if;
		end case;	
		v.sel := sel;

		if icache_in.read = '1' and sel /= sel_insn then
			v.icache_pending := '1';
			v.icache_burstcount := icache_in.burstcount;
		end if;	
		
--		avm_mem_lock <= '0';
		avm_mem_writedata <= dcache_in.writedata;  -- No write from Instruction Cache
		icache_out.readdatavalid <= '0';
		icache_out.waitrequest <= '0'; 
		icache_out.timeout <= '0';
		dcache_out.readdatavalid <= '0'; 
		dcache_out.waitrequest <= '0';
		dcache_out.timeout <= '0';
		mmu_out.readdatavalid <= '0';	
		mmu_out.waitrequest <= '0';
		mmu_out.timeout <= '0';
		case sel is
			when sel_data =>
				avm_mem_address <= dcache_in.address;
				avm_mem_burstcount <= dcache_in.burstcount;
				avm_mem_read <= dcache_in.read;
				avm_mem_write <= dcache_in.write;	 
				avm_mem_byteenable <= dcache_in.byteenable;
--				avm_mem_lock <= dcache_in.locked;
				dcache_out.readdatavalid <= avm_mem_readdatavalid;
				dcache_out.waitrequest <= avm_mem_waitrequest;
				icache_out.waitrequest <= icache_in.read;
				icache_out.timeout <= vtimeout;
		  when sel_insn =>	
				avm_mem_address <= icache_in.address;
				avm_mem_burstcount <= icache_burstcount;
				avm_mem_read <= icache_in.read or r.icache_pending;
				avm_mem_write <= '0';
				avm_mem_byteenable <= (others => '1');
				icache_out.readdatavalid <= avm_mem_readdatavalid;																
				icache_out.waitrequest <= avm_mem_waitrequest;
				dcache_out.waitrequest <= dcache_in.read or dcache_in.write;
				dcache_out.timeout <= vtimeout;
			when sel_mmu =>
				avm_mem_address <= mmu_in.address;
				avm_mem_burstcount <= CONV_STD_LOGIC_VECTOR(1, avm_mem_burstcount'length);
				avm_mem_read <= mmu_in.read;
				avm_mem_write <= '0';
				avm_mem_byteenable <= (others => '1');
				mmu_out.readdatavalid <= avm_mem_readdatavalid;
				mmu_out.waitrequest <= avm_mem_waitrequest;
				mmu_out.timeout <= vtimeout;
		end case;	
		
		next_r <= v;
	end process next_state_proc;	
	
	process(reset, clk)
	begin
		if reset = '1' then
			r.state <= st_arbiter;
			r.sel <= sel_insn;
			r.icache_pending <= '0';
			r.counter <= (others => '0');
		elsif rising_edge(clk) then	
			r <= next_r;
		end if;
	end process;	

  icache_out.readdata <= avm_mem_readdata;
	dcache_out.readdata <= avm_mem_readdata;
	mmu_out.readdata <= avm_mem_readdata;
end behaviour;
