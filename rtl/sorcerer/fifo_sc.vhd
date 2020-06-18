-- Project Sorcerer - RISC-V core
--
-- Single-clock FIFO
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

-- Single clock FIFO
entity fifo_sc is
	generic
	(
	  WIDTH : natural;
		DEPTH : natural;
		DEVICE_FAMILY : string := ""
	);
	port
	(
	  clk: in std_logic;
	  reset : in std_logic;
		data : in std_logic_vector(WIDTH-1 downto 0);
		rdreq	: in std_logic;
		wrreq	: in std_logic;	
		flush : in std_logic;
		empty	: out std_logic;
		full : out std_logic;
		q	: out std_logic_vector(WIDTH-1 downto 0);
		usedw	: out std_logic_vector(ceil_log2(DEPTH)-1 downto 0)
	);	
end fifo_sc;

architecture behaviour of fifo_sc is
  constant ADDR_BITS: natural := ceil_log2(DEPTH);

  subtype word_type is std_logic_vector(WIDTH-1 downto 0);
  type ram_type is array (0 to DEPTH-1) of word_type;
  signal ram : ram_type :=(others => (others => '0')); 
	
	type fifo_regs_type is record
    read_ptr: std_logic_vector(ADDR_BITS-1 downto 0);
		write_ptr: std_logic_vector(ADDR_BITS-1 downto 0);
		word_num: std_logic_vector(ADDR_BITS-1 downto 0);
	end record;
	
	signal r, next_r: fifo_regs_type;
begin	
	fifo_ram_proc: process(clk)
	begin
		if rising_edge(clk) then
			if wrreq = '1' and r.word_num /= DEPTH - 1 then
				ram(CONV_INTEGER(r.write_ptr)) <= data;
			end if;
		end if;
	end process fifo_ram_proc;
	q <= ram(CONV_INTEGER(r.read_ptr));	
	
	next_state_proc: process(r, rdreq, wrreq)
	  variable v: fifo_regs_type;
		variable inc_dec: std_logic_vector(1 downto 0);
	
		function next_ptr(ptr: std_logic_vector(ADDR_BITS-1 downto 0)) return std_logic_vector is
		begin
			if ceil_log2(DEPTH) /= log2(DEPTH) then
				if ptr = DEPTH-1 then
					return CONV_STD_LOGIC_VECTOR(0, ADDR_BITS);
				else	
					return ptr + 1;
				end if;
			else
        return ptr + 1;					
			end if;			
		end function next_ptr;	
	begin
	  v := r;	
		inc_dec := "00";
		
		if rdreq = '1' then
			if r.word_num /= 0 then
				v.read_ptr := next_ptr(r.read_ptr);
				inc_dec(0) := '1';
			end if;	
		end if;
		if wrreq = '1' then
			if v.word_num /= DEPTH-1 or rdreq = '1' then
				v.write_ptr := next_ptr(r.write_ptr);
				inc_dec(1) := '1';
			end if;			
		end if;	
		case inc_dec is
			when "10" =>  v.word_num := r.word_num + 1;
			when "01" => v.word_num := r.word_num - 1;
			when others => null;
		end case;	
		if flush = '1' then
			v.read_ptr := (others => '0');
			v.write_ptr := (others => '0');
			v.word_num := (others => '0');			
		end if;				
		next_r <= v;
	end process next_state_proc;	
	
	process(reset, clk)
	begin
		if reset = '1' then
			r.read_ptr <= (others => '0');
			r.write_ptr <= (others => '0');
			r.word_num <= (others => '0');
		elsif rising_edge(clk) then	
			r <= next_r;
		end if;
	end process;	
	
  usedw <= r.word_num;
	empty <= '1' when r.word_num = 0 else '0';
	full <= '1' when r.word_num = DEPTH - 1 else '0';	
end behaviour;	

library altera_mf;
use altera_mf.altera_mf_components.all;

architecture altera of fifo_sc is
begin
	scfifo_component : scfifo
	generic map
	(
		add_ram_output_register => "OFF",
		intended_device_family => DEVICE_FAMILY,
		lpm_numwords => DEPTH,
		lpm_showahead => "ON",
		lpm_type => "scfifo",
		lpm_width => WIDTH,
		lpm_widthu => ceil_log2(DEPTH),
		overflow_checking => "OFF",
		underflow_checking => "OFF",
		use_eab => "ON"
	)
	port map 
	(
		clock => clk,
		data => data,
		rdreq => rdreq,
		wrreq => wrreq,
		aclr => reset,
		sclr => flush,
		usedw => usedw,
		empty => empty,
		full => full,
		q => q
	);
end altera;
