-- Project Sorcerer - RISC-V core
--
-- Trace Module FIFO data buffer
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
use work.utils.all;
use work.pipeline_iface.all;
use work.iface.all;

entity trace_fifo is
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
end trace_fifo;

architecture behaviour of trace_fifo is
  for all : fifo_sc use entity work.fifo_sc(altera);

  constant FIFO_BITS: natural := ceil_log2(FIFO_DEPTH);
	constant LOW_BITS: natural := log2(FIFO_WIDTH/OUT_WIDTH);

  type fifo_regs_type is record
		remain_trace: std_logic_vector(FIFO_WIDTH-2 downto 0);
		remain_num: std_logic_vector(ceil_log2(FIFO_WIDTH-1) downto 0);
		num_low: std_logic_vector(LOW_BITS-1 downto 0);
	end record;
	
  signal r, next_r: fifo_regs_type;	
	
  signal fifo_data: std_logic_vector(FIFO_WIDTH-1 downto 0);
	signal fifo_rdreq: std_logic;
  signal fifo_wrreq: std_logic;
  signal fifo_empty: std_logic;
  signal fifo_full: std_logic;
  signal fifo_q: std_logic_vector(FIFO_WIDTH-1 downto 0);
  signal fifo_usedw: std_logic_vector(FIFO_BITS-1 downto 0);	
begin
  fifo1: fifo_sc
	generic	map
	(
	  WIDTH => FIFO_WIDTH,
		DEPTH => FIFO_DEPTH,
		DEVICE_FAMILY => DEVICE_FAMILY
	)
	port map
	(
	  clk => clk,
	  reset => reset,
		data => fifo_data,
		rdreq	=> fifo_rdreq,
		wrreq	=> fifo_wrreq,
		flush => flush,
		empty	=> fifo_empty,
		full => fifo_full,
		q	=> fifo_q,
		usedw	=> fifo_usedw
	);	
	
	next_state_proc: process(r, trace, trace_len, trace_wrreq, fifo_usedw, bus_rdreq)
	  variable v: fifo_regs_type;
		variable shift_out: std_logic_vector(2*TRACE_WIDTH-1 downto 0);
		variable shift_trace: std_logic_vector(FIFO_WIDTH+TRACE_WIDTH-1 downto 0);
		variable merge_data: std_logic_vector(FIFO_WIDTH+TRACE_WIDTH-1 downto 0);	
		variable wrreq: std_logic;
		variable remain_num: std_logic_vector(ceil_log2(FIFO_WIDTH+TRACE_WIDTH-1) downto 0);
		variable next_remain_num: std_logic_vector(ceil_log2(FIFO_WIDTH+TRACE_WIDTH-1) downto 0);
		
	begin
	  v := r;	
		wrreq := '0';
		
		shift_out := barrel_shift_l(trace, r.remain_num)(shift_out'range);
		shift_trace := (others => '0');
		shift_trace(shift_out'range) := shift_out;
		merge_data(merge_data'high downto FIFO_WIDTH-1) := (others => '0');
		merge_data(FIFO_WIDTH-2 downto 0) := r.remain_trace;
		merge_data := merge_data or shift_trace;
		
		fifo_data <= merge_data(FIFO_WIDTH-1 downto 0);
		if trace_wrreq = '1' then
			remain_num := (others => '0');
			remain_num(trace_len'range) := trace_len;
			remain_num := remain_num + r.remain_num; 
			next_remain_num := remain_num - FIFO_WIDTH;
			if next_remain_num(next_remain_num'high) = '0' then -- remain_num > FIFO_WIDTH then
				wrreq := '1';
				remain_num := next_remain_num(remain_num'range);
			else	
				wrreq := '0';				
			end if;	
			v.remain_num := remain_num(v.remain_num'range);
			if wrreq = '1' then
				v.remain_trace(TRACE_WIDTH-1 downto 0) := merge_data(merge_data'high downto FIFO_WIDTH);
				if FIFO_WIDTH-2 >= TRACE_WIDTH then
				  v.remain_trace(FIFO_WIDTH-2 downto TRACE_WIDTH) := (others => '0');
				end if;	
			else	
			  v.remain_trace := merge_data(FIFO_WIDTH-2 downto 0);	
			end if;
		end if;
		fifo_rdreq <= '0';
		if FIFO_WIDTH > OUT_WIDTH then
			if bus_rdreq = '1' then
				v.num_low := r.num_low + 1;	
				if r.num_low = CONV_STD_LOGIC_VECTOR(FIFO_WIDTH/OUT_WIDTH-1, LOW_BITS) then
					fifo_rdreq <= '1';
				end if;	
			end if;	
			q <= (others => '0');
			for i in 0 to FIFO_WIDTH/OUT_WIDTH - 1 loop
				if i = r.num_low then
					q <= fifo_q(OUT_WIDTH*(i+1)-1 downto OUT_WIDTH*i);
				end if;	
			end loop;	
			num_word(LOW_BITS-1 downto 0) <= CONV_STD_LOGIC_VECTOR(0, LOW_BITS) - r.num_low;
			if r.num_low = 0 then
				num_word(num_word'high downto LOW_BITS) <= fifo_usedw;
			else	
				num_word(num_word'high downto LOW_BITS) <= fifo_usedw - 1;
			end if;				
		else
			fifo_rdreq <= bus_rdreq;
			q <= fifo_q;
			num_word <= fifo_usedw;
		end if;
		
		fifo_wrreq <= wrreq;
		next_r <= v;
	end process next_state_proc;

	full <= fifo_full;
	
	process(reset, clk)
	begin
		if reset = '1' then	
			r.remain_trace <= (others => '0'); 
			r.remain_num <= (others => '0');
			r.num_low <= (others => '0');
		elsif rising_edge(clk) then	
			r <= next_r;
		end if;
	end process;
	
end behaviour;
