-- Project Sorcerer - RISC-V core
--
-- Instruction Buffer core module
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
use work.utils.all;

entity ibuffer_core is
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
end ibuffer_core;

architecture behaviour of ibuffer_core is
  type reg_type is record
	  valid: std_logic_vector(BUFFER_DEPTH-1 downto 0);
		slices: std_logic_2d_type(0 to BUFFER_DEPTH-1, SLICE_WIDTH-1 downto 0);
  end record;	
	
	signal r, next_r: reg_type;
begin
	next_state_proc: process(r, data, data_valid, flush, insn_size)
	  variable v: reg_type;
		variable valid_shifted: std_logic_vector(BUFFER_DEPTH downto 0); 
		variable fetch_select: std_logic_2d_type(0 to FETCH_SLICE_NUM-1, BUFFER_DEPTH-1 downto 0);
		variable fifo_select: std_logic_2d_type(0 to INSN_SLICE_NUM, BUFFER_DEPTH-1 downto 0);
		variable overflow: std_logic;	
	begin
		v := r;
		
		valid_shifted(0) := '1';
		valid_shifted(BUFFER_DEPTH downto 1) := barrel_shift_r(r.valid, insn_size);

		-- buffer overflow detection
		overflow := '0';
		for i in 0 to FETCH_SLICE_NUM-1 loop
			overflow := overflow or (data_valid(i) and valid_shifted(valid_shifted'high-i)); 
		end loop;	
		
		-- Slice FIFO input multiplexor control
		for i in 0 to INSN_SLICE_NUM loop
			for k in 0 to BUFFER_DEPTH-1 loop
				fifo_select(i, k) := '0';
			end loop;
			if i = insn_size then
				for k in 0 to BUFFER_DEPTH-1 loop
					fifo_select(i, k) := valid_shifted(k + 1);
				end loop;	
			end if;	
		end loop;	
		
		-- Slice fetch input multiplexor control
		for k in 0 to BUFFER_DEPTH-1 loop
			fetch_select(0, k) := (not valid_shifted(k+1)) and valid_shifted(k);
		end loop;	
		for i in 1 to INSN_SLICE_NUM-1 loop
			fetch_select(i, 0) := '0';
			for k in 1 to BUFFER_DEPTH-1 loop
				fetch_select(i, k) := fetch_select(i-1, k-1);
			end loop;	
		end loop;	
		
		-- Buffer slices multiplexer
		for i in 0 to BUFFER_DEPTH-1 loop
			v.valid(i) := '0';
			for k in 0 to SLICE_WIDTH-1 loop
			  v.slices(i, k) := '0';
			end loop;
			
			for j in 0 to FETCH_SLICE_NUM-1 loop
				if fetch_select(j, i) = '1' then
			    for k in 0 to SLICE_WIDTH-1 loop
			      v.slices(i, k) := data(j, k);
			    end loop;	
					if flush = '0' and overflow = '0' then
            v.valid(i) := data_valid(j);  					
					end if;	
				end if;	
			end loop;	
			
			for j in 0 to imin(INSN_SLICE_NUM, BUFFER_DEPTH-i-1) loop
				if fifo_select(j, i) = '1' then
			    for k in 0 to SLICE_WIDTH-1 loop
			      v.slices(i, k) := r.slices(i+j, k);
			    end loop;
					if flush = '0' then
            v.valid(i) := r.valid(i+j);
					end if;	
				end if;	
			end loop;			
		end loop;	

		for i in 0 to INSN_SLICE_NUM-1 loop
			for k in 0 to SLICE_WIDTH-1 loop 
				if UNREGISTERED_OUT = 0 then
				  insn(i, k) <= r.slices(i, k);
				else
					insn(i, k) <= v.slices(i, k);
				end if;	
			end loop;					 
			if UNREGISTERED_OUT = 0 then
			  insn_valid(i) <= r.valid(i);
			else
				insn_valid(i) <= v.valid(i);
			end if;	
		end loop;	

		busy <= overflow or hard_stall;
		next_r <= v;
	end process next_state_proc;		

	process(reset, clk)
	begin
		if reset = '1' then
			for i in 0 to BUFFER_DEPTH-1 loop
				r.valid(i) <= '0';
			end loop;
		elsif rising_edge(clk) then	
			if hard_stall = '0' then 
			  r <= next_r; 
			end if;
		end if;	
	end process;	
end behaviour;
