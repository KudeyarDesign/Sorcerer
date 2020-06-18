-- Project Sorcerer - RISC-V core
--
-- Instruction Buffer for aligning fetched instructions before decoding
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
use work.pipeline_iface.all;
use work.iface.all;

entity ibuffer is
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
end ibuffer;

architecture behaviour of ibuffer is
  constant FETCH_SLICE_NUM: natural := ICACHE_FETCH_WIDTH/16;

	signal data: std_logic_2d_type(0 to FETCH_SLICE_NUM-1, 15 downto 0);
	signal data_valid: std_logic_vector(FETCH_SLICE_NUM-1 downto 0);  
	signal insn_size: std_logic_vector(1 downto 0);
	signal insn: std_logic_2d_type(0 to 1, 15 downto 0);  
	signal insn_valid: std_logic_vector(1 downto 0);
	signal insn_pc, next_insn_pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
begin
	ibuf_core1: ibuffer_core
	generic map
	(
	  SLICE_WIDTH => 16,
		FETCH_SLICE_NUM => FETCH_SLICE_NUM,
		BUFFER_DEPTH => BUFFER_DEPTH,
		INSN_SLICE_NUM => INSN_SLICE_NUM
	)
	port map
	(
		clk => clk,
		reset => reset,
		data => data,
		data_valid => data_valid,
		flush => din.flush,
		hard_stall => din.hard_stall,
		insn_size => insn_size,
		busy => dout.busy,
		insn => insn,
		insn_valid => insn_valid
	);

	mux_in: process(din)
	  variable pc_low: std_logic_vector(log2(FETCH_SLICE_NUM)-1 downto 0);
		variable shift_data: std_logic_vector(ICACHE_FETCH_WIDTH-1 downto 0);
		variable valid: std_logic_vector(FETCH_SLICE_NUM-1 downto 0);
		variable data_col, data_col_shifted: std_logic_vector(FETCH_SLICE_NUM-1 downto 0);
	begin
		pc_low := din.pc(pc_low'high+1 downto 1);
		if din.icache_ready = '1' then
		  valid := (others => '1');
		else
			valid := (others => '0');
		end if;	
		data_valid <= barrel_shift_r(valid, pc_low);
		
		for i in 0 to 15 loop
			for k in 0 to FETCH_SLICE_NUM-1 loop
				data_col(k) := din.icache_data(k*16 + i);
			end loop;
			data_col_shifted := barrel_shift_r(data_col, pc_low);
			for k in 0 to FETCH_SLICE_NUM-1 loop
				data(k, i) <= data_col_shifted(k);
			end loop;	
		end loop;	 
		
	end process;	
	
  insn_size <= "10" when din.stall = '0' and insn_valid(1) = '1' and ((insn(0, 0) and insn(0,1)) = '1') else
	             "01" when din.stall = '0' and insn_valid(0) = '1' and ((insn(0, 0) and insn(0,1)) = '0') else
							 "00";
  next_insn_pc <= din.pc when insn_valid(0) = '0' else insn_pc + (insn_size & "0");

	process(clk)
	begin
		if rising_edge(clk) then
			insn_pc <= next_insn_pc;
		end if;	
	end process;	
	
	outp: for n in 0 to INSN_SLICE_NUM-1 generate
	  outpn: for i in 0 to 15 generate
			dout.insn(n*16 + i) <= insn(n, i);
		end generate;	
	end generate;
	dout.ready <= insn_valid;
	dout.pc <= insn_pc;
	dout.page_fault <= (others => '0');  -- TODO
end behaviour;
