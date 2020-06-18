-- Project Sorcerer - RISC-V core
--
-- Virtual JTAG IP wrapper
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

LIBRARY altera_mf;
USE altera_mf.all;

ENTITY dbg_vjtag IS
	PORT
	(
		ir_out		: in std_logic_vector(4 downto 0);
		tdo		: in std_logic;
		ir_in		: out std_logic_vector(4 downto 0);
		tck		: out std_logic;
		tdi		: out std_logic;
		virtual_state_cdr		: out std_logic;
		virtual_state_cir		: out std_logic;
		virtual_state_e1dr	: out std_logic;
		virtual_state_e2dr	: out std_logic;
		virtual_state_pdr		: out std_logic;
		virtual_state_sdr		: out std_logic;
		virtual_state_udr		: out std_logic;
		virtual_state_uir		: out std_logic	
	);
END dbg_vjtag;

ARCHITECTURE altera OF dbg_vjtag IS
	SIGNAL sub_wire0	: STD_LOGIC ;
	SIGNAL sub_wire1	: STD_LOGIC ;
	SIGNAL sub_wire2	: STD_LOGIC_VECTOR (4 DOWNTO 0);
	SIGNAL sub_wire3	: STD_LOGIC ;
	SIGNAL sub_wire4	: STD_LOGIC ;
	SIGNAL sub_wire5	: STD_LOGIC ;
	SIGNAL sub_wire6	: STD_LOGIC ;
	SIGNAL sub_wire7	: STD_LOGIC ;
	SIGNAL sub_wire8	: STD_LOGIC ;
	SIGNAL sub_wire9	: STD_LOGIC ;
	SIGNAL sub_wire10	: STD_LOGIC ;

	COMPONENT sld_virtual_jtag
	GENERIC (
		sld_auto_instance_index		: STRING;
		sld_instance_index		: NATURAL;
		sld_ir_width		: NATURAL;
		sld_sim_action		: STRING;
		sld_sim_n_scan		: NATURAL;
		sld_sim_total_length		: NATURAL;
		lpm_type		: STRING
	);
	PORT (
			virtual_state_cir	: OUT STD_LOGIC ;
			virtual_state_pdr	: OUT STD_LOGIC ;
			ir_in	: OUT STD_LOGIC_VECTOR (4 DOWNTO 0);
			tdi	: OUT STD_LOGIC ;
			virtual_state_udr	: OUT STD_LOGIC ;
			ir_out	: IN STD_LOGIC_VECTOR (4 DOWNTO 0);
			tck	: OUT STD_LOGIC ;
			virtual_state_e1dr	: OUT STD_LOGIC ;
			virtual_state_uir	: OUT STD_LOGIC ;
			tdo	: IN STD_LOGIC ;
			virtual_state_cdr	: OUT STD_LOGIC ;
			virtual_state_e2dr	: OUT STD_LOGIC ;
			virtual_state_sdr	: OUT STD_LOGIC 
	);
	END COMPONENT;

BEGIN
	virtual_state_cir    <= sub_wire0;
	virtual_state_pdr    <= sub_wire1;
	ir_in    <= sub_wire2(4 DOWNTO 0);
	tdi    <= sub_wire3;
	virtual_state_udr    <= sub_wire4;
	tck    <= sub_wire5;
	virtual_state_e1dr    <= sub_wire6;
	virtual_state_uir    <= sub_wire7;
	virtual_state_cdr    <= sub_wire8;
	virtual_state_e2dr    <= sub_wire9;
	virtual_state_sdr    <= sub_wire10;

	sld_virtual_jtag_component : sld_virtual_jtag
	GENERIC MAP (
		sld_auto_instance_index => "YES",
		sld_instance_index => 0,
		sld_ir_width => 5,
		sld_sim_action => "",
		sld_sim_n_scan => 0,
		sld_sim_total_length => 0,
		lpm_type => "sld_virtual_jtag"
	)
	PORT MAP (
		ir_out => ir_out,
		tdo => tdo,
		virtual_state_cir => sub_wire0,
		virtual_state_pdr => sub_wire1,
		ir_in => sub_wire2,
		tdi => sub_wire3,
		virtual_state_udr => sub_wire4,
		tck => sub_wire5,
		virtual_state_e1dr => sub_wire6,
		virtual_state_uir => sub_wire7,
		virtual_state_cdr => sub_wire8,
		virtual_state_e2dr => sub_wire9,
		virtual_state_sdr => sub_wire10
	);
END altera;
