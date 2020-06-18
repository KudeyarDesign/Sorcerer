-- Project Sorcerer - RISC-V core
--
-- 32-bit hardware Divider
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

entity div32x32 is
	port
	(	 
		clk : in std_logic;
		reset : in std_logic;	  
	  din: in div_in_type;
		dout: out div_out_type
	);	
end div32x32;

architecture behaviour of div32x32 is	 
	constant SIZE: natural := WORD_SIZE;

  constant MIN_INTEGER: std_logic_vector(SIZE-1 downto 0) := '1' & CONV_STD_LOGIC_VECTOR(0, SIZE-1);
	constant ZERO: std_logic_vector(WORD_SIZE-1 downto 0) := (others => '0');
	constant MINUS_ONE: std_logic_vector(SIZE-1 downto 0) := (others => '1');

  type state_type is (st_idle, st_div, st_correct);
	type regs_type is record
		state: state_type;
		count: std_logic_vector(log2(SIZE)-1 downto 0); 
		af : std_logic_vector(SIZE-1 downto 0);	 -- numerator & remainder
		aq : std_logic_vector(SIZE-1 downto 0);  -- quotient
		ax : std_logic_vector(SIZE-1 downto 0);  -- denominator	
		overflow: std_logic;
		denom_zero: std_logic; 
		remain: std_logic;
		neg: std_logic;
	end record;	
	
	signal r, next_r: regs_type;
	
  signal rs1_sign: std_logic;
  signal rs2_sign: std_logic;

  signal denom_zero: std_logic;
  signal overflow: std_logic;

begin
  rs1_sign <= din.sign and din.rs1(SIZE-1);
	rs2_sign <= din.sign and din.rs2(SIZE-1);

  denom_zero <= '1' when din.rs2 = 0 else '0';
  overflow <= '1' when din.rs1 = MIN_INTEGER and din.rs2 = MINUS_ONE and din.sign = '1'
	  else '0';
	
	next_state: process(din, r, rs1_sign, rs2_sign, denom_zero, overflow)
	  variable v: regs_type; 
		variable af: std_logic_vector(SIZE downto 0);
		variable diff: std_logic_vector(SIZE downto 0);	
		variable mux_out: std_logic_vector(SIZE-1 downto 0);
		variable result: std_logic_vector(SIZE-1 downto 0);
		variable ready: std_logic;
		
		procedure check_special is
		begin
			if r.denom_zero = '1' then
				result := MINUS_ONE;
				if r.remain = '1' then
					v.aq := din.rs1;
					v.state := st_correct;
				else	
					v.state := st_idle;
					ready := '1';
				end if;	
			elsif r.overflow = '1' then
				if r.remain = '0' then
				  result := MIN_INTEGER;
				else
					result := (others => '0');
				end if;	 
				v.state := st_idle;
				ready := '1';
			end if;	
		end procedure check_special;	
	begin										
		v := r;	
		result := (others => '0');
		ready := '0';
		case r.state is
			when st_idle =>
			  v.overflow := overflow;
				v.denom_zero := denom_zero;
				v.remain := din.remain;	 
				if (din.remain = '1' and rs1_sign = '1' and denom_zero = '0') or
					 (din.remain = '0' and (rs1_sign xor rs2_sign) = '1') then
					v.neg := '1';
				else	
					v.neg := '0';
				end if;	
			  if din.start = '1' then	 
					ready := '0';
					if rs1_sign = '0' then
						v.aq := din.rs1;
					else	
						v.aq := ZERO - din.rs1;
					end if;	
					if rs2_sign = '0' then
						v.ax := din.rs2;
					else	
						v.ax := ZERO - din.rs2;
					end if;	
					v.af := (others => '0');
					v.count := (others => '1');
					v.state := st_div;
				else
					ready := '1';
				end if;	 
			when st_div => 
				af := '0' & r.af(SIZE-2 downto 0) & r.aq(SIZE-1);
				diff := af - ('0' & r.ax);
				if diff(diff'high) = '0' then
					v.af := diff(SIZE-1 downto 0);
					v.aq := r.aq(SIZE-2 downto 0) & '1';
				else	
					v.af := af(SIZE-1 downto 0);
					v.aq := r.aq(SIZE-2 downto 0) & '0';
				end if;	
				if din.flush = '1' then
					v.state := st_idle;
				elsif r.count /= 0 then
					v.count := r.count - 1;
				else	
					v.state := st_correct;
				end if;			
				check_special;
			when st_correct =>
			  if r.remain = '1' and r.denom_zero = '0' then
					mux_out := r.af;
				else	
					mux_out := r.aq;
				end if;	
				if r.neg = '1' then
					result := ZERO - mux_out;
				else
					result := mux_out;
				end if;	 
				ready := '1';
			  v.state := st_idle;
		end case;
		dout.ready <= ready;
		dout.result <= result;
		next_r <= v;
	end process next_state;	
	
	process(reset, clk)
	begin
		if reset = '1' then
			r.state <= st_idle;
		elsif rising_edge(clk) then	
			r <= next_r;
		end if;
	end process;	
end behaviour;
