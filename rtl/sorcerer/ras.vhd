-- Project Sorcerer - RISC-V core
--
-- Return Adderss Stack (RAS)
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

entity ras is
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
end ras;

architecture behaviour of ras is
  constant INDEX_BITS: natural := ceil_log2(DEPTH);
 
 	type ras_regs_type is record
		cur_ptr: std_logic_vector(ceil_log2(DEPTH+1)-1 downto 0);
		num_words: std_logic_vector(INDEX_BITS-1 downto 0); 
	end record;	 
	signal r, next_r: ras_regs_type;
	
	type ram_type is array (0 to DEPTH-1) of std_logic_vector(VADDRESS_WIDTH-1 downto 0);
	signal ram: ram_type;
	signal ram_q: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
begin	 
	ras_ram_proc: process(clk)
	begin
		if rising_edge(clk) then
      if din.push = '1' then
        ram(CONV_INTEGER(next_r.cur_ptr)) <= din.pc;
      end if;
    ram_q <= ram(CONV_INTEGER(r.cur_ptr));
		end if;
  end process ras_ram_proc;

	dout.top_pc <= (ram_q(ram_q'high downto 1) & "0") when ENABLE_C = 1 
	     else (ram_q(ram_q'high downto 2) & "00"); 
	
	next_state_proc: process(r, din)
	  variable v: ras_regs_type;
		variable is_empty: std_logic;
	begin
		v := r;					 
		if r.num_words = 0 then
			is_empty := '1';
		else	
			is_empty := '0';
		end if;	
		if din.flush = '1' then
			v.num_words := (others => '0');
		elsif din. push = '1' then
			if r.num_words < DEPTH then
				v.num_words := r.num_words + 1;
			end if;	
			if r.cur_ptr = DEPTH-1 then
				v.cur_ptr := (others => '0');				
			else	
				v.cur_ptr := r.cur_ptr + 1;
			end if;	
		elsif din.pop = '1' then
			if is_empty = '0' then
				v.num_words := r.num_words - 1;
				if r.cur_ptr = 0 then
					v.cur_ptr := CONV_STD_LOGIC_VECTOR(DEPTH-1, INDEX_BITS);
				else
					v.cur_ptr := r.cur_ptr - 1;
				end if;	
			end if;	
		end if;	
		
		dout.empty <= is_empty;
		next_r <= v;
	end process next_state_proc;	
	
	process(reset, clk)
	begin
		if reset = '1' then
			r.cur_ptr <= (others => '0');
			r.num_words <= (others => '0');
		elsif rising_edge(clk) then	
			r <= next_r;
		end if;	
	end process;
end behaviour;
