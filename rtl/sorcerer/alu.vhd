-- Project Sorcerer - RISC-V core
--
-- Arithmetic & Logical Unit
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
use work.pipeline_iface.all;
use work.iface.all;

entity alu is
	generic
	(
	  ENABLE_SHIFTER: natural range 0 to 1 := 0  -- 1 - standalone shifter, 0 - shifter through multiplier
	);
	port
	(
	  din: in alu_in_type;
		dout: out alu_out_type
	);
end alu;

architecture behaviour of alu is 
  signal shift_right: std_logic;
	signal shift_sign: std_logic;
	signal shift_result: std_logic_vector(WORD_SIZE-1 downto 0);
begin										
	shift_on: if ENABLE_SHIFTER = 1 generate
  shift1: barrel_shifter
	port map
	(
	  a => din.rs1,
		amount => din.rs2(7 downto 0),
		right => shift_right,
		sign => shift_sign,
		result => shift_result
	);	
	
	shift_right <= '1' when din.oper = alu_srl or din.oper = alu_sra else '0';
	shift_sign <= '1' when din.oper = alu_sra else '0';	
	end generate;
	
  alu_proc: process(din) 
	  variable v: alu_out_type;
		variable add: std_logic_vector(WORD_SIZE downto 0);
		variable sub: std_logic_vector(WORD_SIZE downto 0);
	begin	
		v.result := din.rs1;
		v.flags.v := '0';
		add := ('0' & din.rs1) + ('0' & din.rs2);
		sub := ('0' & din.rs1) - ('0' & din.rs2);
		if sub(WORD_SIZE-1 downto 0) = 0 then
			v.flags.z := '1';
		else	
			v.flags.z := '0';	
		end if;	
		v.flags.n := sub(WORD_SIZE-1);
		v.flags.c := sub(WORD_SIZE);
		case din.oper is		
	    when alu_add =>
				v.result := add(WORD_SIZE-1 downto 0);
				v.flags.v := (din.rs2(WORD_SIZE-1) and din.rs1(WORD_SIZE-1) and not v.result(WORD_SIZE-1)) or 
				        (not din.rs2(WORD_SIZE-1) and not din.rs1(WORD_SIZE-1) and v.result(WORD_SIZE-1));				
	    when alu_and =>
				v.result := din.rs1 and din.rs2;
			when alu_mov =>
			  v.result := din.rs1;
			when alu_mov2 =>
			  v.result := din.rs2;			
		  when alu_or =>
			  v.result := din.rs1 or din.rs2;
			when alu_slt =>
				v.flags.v := (din.rs2(WORD_SIZE-1) and din.rs1(WORD_SIZE-1) and not v.result(WORD_SIZE-1)) or 
				        (not din.rs2(WORD_SIZE-1) and not din.rs1(WORD_SIZE-1) and v.result(WORD_SIZE-1));			
				v.result(0) := sub(WORD_SIZE) xor v.flags.v; 
				v.result(WORD_SIZE-1 downto 1) := (others => '0');
			when alu_sltu =>
				v.result(0) := sub(WORD_SIZE);
				v.result(WORD_SIZE-1 downto 1) := (others => '0');
      when alu_sub =>
				v.result := sub(WORD_SIZE-1 downto 0);
				v.flags.v := (din.rs2(WORD_SIZE-1) and din.rs1(WORD_SIZE-1) and not v.result(WORD_SIZE-1)) or 
				           (not din.rs2(WORD_SIZE-1) and not din.rs1(WORD_SIZE-1) and v.result(WORD_SIZE-1));			
		  when alu_xor =>		
			  v.result := din.rs1 xor din.rs2;
			when alu_sll | alu_srl | alu_sra =>
			  if ENABLE_SHIFTER = 1 then
					v.result := shift_result;
				end if;
			when others =>  null;
		end case;

		-- Fast output flags for conditional branch operations
		v.br_flags.n := sub(WORD_SIZE-1);
		v.br_flags.c := sub(WORD_SIZE);
		if din.rs1 = din.rs2 then
			v.br_flags.z := '1';
		else	
			v.br_flags.z := '0';
		end if;	
		v.br_flags.v := (din.rs2(WORD_SIZE-1) and din.rs1(WORD_SIZE-1) and not sub(WORD_SIZE-1)) or 
		                (not din.rs2(WORD_SIZE-1) and not din.rs1(WORD_SIZE-1) and sub(WORD_SIZE-1));	
		dout <= v;
	end process alu_proc;
end behaviour;
