-- Project Sorcerer - RISC-V core
--
-- Multiplier 16 x 16, operation during 1 cycle
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

LIBRARY ieee;
USE ieee.std_logic_1164.all;
use work.config.all;

LIBRARY altera_mf;
USE altera_mf.all;

entity mul16x16 is
	port
	(
		dataa_0: in std_logic_vector(15 downto 0) := (others => '0');
		datab_0: in std_logic_vector(15 downto 0) := (others => '0');
		signa: in std_logic := '0';
		signb: in std_logic := '0';
		result: out std_logic_vector(31 downto 0)
	);
end mul16x16;

ARCHITECTURE altera OF mul16x16 IS
	SIGNAL sub_wire0	: STD_LOGIC_VECTOR (31 DOWNTO 0);

	COMPONENT altmult_add
	GENERIC 
	(
		addnsub_multiplier_aclr1		: STRING;
		addnsub_multiplier_pipeline_aclr1		: STRING;
		addnsub_multiplier_pipeline_register1		: STRING;
		addnsub_multiplier_register1		: STRING;
		dedicated_multiplier_circuitry		: STRING;
		input_register_a0		: STRING;
		input_register_b0		: STRING;
		input_source_a0		: STRING;
		input_source_b0		: STRING;
		intended_device_family		: STRING;
		lpm_type		: STRING;
		multiplier1_direction		: STRING;
		multiplier_register0		: STRING;
		number_of_multipliers		: NATURAL;
		output_register		: STRING;
		port_addnsub1		: STRING;
		port_signa		: STRING;
		port_signb		: STRING;
		signed_pipeline_register_a		: STRING;
		signed_pipeline_register_b		: STRING;
		signed_register_a		: STRING;
		signed_register_b		: STRING;
		width_a		: NATURAL;
		width_b		: NATURAL;
		width_result		: NATURAL
	);
	PORT 
	(
			datab	: IN STD_LOGIC_VECTOR (15 DOWNTO 0);
			signa	: IN STD_LOGIC ;
			dataa	: IN STD_LOGIC_VECTOR (15 DOWNTO 0);
			result	: OUT STD_LOGIC_VECTOR (31 DOWNTO 0);
			signb	: IN STD_LOGIC 
	);
	END COMPONENT;

BEGIN
	result    <= sub_wire0(31 DOWNTO 0);

	ALTMULT_ADD_component : ALTMULT_ADD
	GENERIC MAP 
	(
		addnsub_multiplier_aclr1 => "UNUSED",
		addnsub_multiplier_pipeline_aclr1 => "UNUSED",
		addnsub_multiplier_pipeline_register1 => "CLOCK0",
		addnsub_multiplier_register1 => "CLOCK0",
		dedicated_multiplier_circuitry => "AUTO",
		input_register_a0 => "UNREGISTERED",
		input_register_b0 => "UNREGISTERED",
		input_source_a0 => "DATAA",
		input_source_b0 => "DATAB",
		intended_device_family => DEVICE_FAMILY,
		lpm_type => "altmult_add",
		multiplier1_direction => "ADD",
		multiplier_register0 => "UNREGISTERED",
		number_of_multipliers => 1,
		output_register => "UNREGISTERED",
		port_addnsub1 => "PORT_UNUSED",
		port_signa => "PORT_USED",
		port_signb => "PORT_USED",
		signed_pipeline_register_a => "UNREGISTERED",
		signed_pipeline_register_b => "UNREGISTERED",
		signed_register_a => "UNREGISTERED",
		signed_register_b => "UNREGISTERED",
		width_a => 16,
		width_b => 16,
		width_result => 32
	)
	PORT MAP 
	(
		datab => datab_0,
		signa => signa,
		dataa => dataa_0,
		signb => signb,
		result => sub_wire0
	);
end altera;

