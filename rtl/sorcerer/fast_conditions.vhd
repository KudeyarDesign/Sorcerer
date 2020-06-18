-- Project Sorcerer - RISC-V core
--
-- Fast operation output conditions calculation
-- Use to bypass ALU conditions calculation
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
use work.config.all;
use work.pipeline_iface.all;

entity fast_conditions is
  port
	(
	  rs1: in std_logic_vector(WORD_SIZE-1 downto 0);
		rs2: in std_logic_vector(WORD_SIZE-1 downto 0);
		flags: out condition_flags
	);
end fast_conditions;

library lpm;
use lpm.all;

architecture altera of fast_conditions is
	component lpm_compare
	generic 
	(
		lpm_representation		: STRING;
		lpm_type		: STRING;
		lpm_width		: NATURAL
	);
	port 
	(
			dataa	: IN STD_LOGIC_VECTOR (WORD_SIZE-1 DOWNTO 0);
			datab	: IN STD_LOGIC_VECTOR (WORD_SIZE-1 DOWNTO 0);
			aeb	: OUT STD_LOGIC ;
			ageb	: OUT STD_LOGIC ;
			alb	: OUT STD_LOGIC ;
			aneb	: OUT STD_LOGIC 
	);
	end component;
begin
	unsig : LPM_COMPARE
	generic map 
	(
		lpm_representation => "UNSIGNED",
		lpm_type => "LPM_COMPARE",
		lpm_width => WORD_SIZE
	)
	port map 
	(
		dataa => rs1,
		datab => rs2,
		aeb => flags.eq,
		ageb => flags.geu,
		alb => flags.ltu,
		aneb => flags.ne
	);
	
	sig : LPM_COMPARE
	generic map 
	(
		lpm_representation => "SIGNED",
		lpm_type => "LPM_COMPARE",
		lpm_width => WORD_SIZE
	)
	port map 
	(
		dataa => rs1,
		datab => rs2,
		ageb => flags.ge,
		alb => flags.lt
	);	
end altera;
