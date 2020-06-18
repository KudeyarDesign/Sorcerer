-- Project Sorcerer - RISC-V core
--
-- Display Module FIFO
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

library altera_mf;
use altera_mf.altera_mf_components.all;

use work.utils.all;

entity display_fifo is
	generic
	(
	  WIDTH: natural range 8 to 128 := 32;
		DEPTH: natural range 16 to 8192 := 128;
		DEVICE_FAMILY: string := "Cyclone V"
	); 
  port
  (
		data		: in std_logic_vector(WIDTH-1 downto 0);
		wrreq		: in std_logic;
		rdreq		: in std_logic;
		rdclk		: in std_logic;
		wrclk		: in std_logic;
		aclr		: in std_logic;
		q		: out std_logic_vector(WIDTH-1 downto 0);
		rdempty		: out std_logic;
		wrusedw		: out std_logic_vector(ceil_log2(DEPTH)-1 downto 0)
	);	
end display_fifo;

ARCHITECTURE SYN OF display_fifo IS
	SIGNAL sub_wire0	: STD_LOGIC ;
	SIGNAL sub_wire1	: STD_LOGIC_VECTOR (ceil_log2(DEPTH)-1 DOWNTO 0);
	SIGNAL sub_wire2	: STD_LOGIC_VECTOR (WIDTH-1 DOWNTO 0);

	COMPONENT dcfifo
	GENERIC 
	(
		intended_device_family		: STRING;
		lpm_width		: NATURAL;
		lpm_numwords		: NATURAL;
		lpm_widthu		: NATURAL;
		clocks_are_synchronized		: STRING;
		lpm_type		: STRING;
		lpm_showahead		: STRING;
		overflow_checking		: STRING;
		underflow_checking		: STRING;
		use_eab		: STRING;
		add_ram_output_register		: STRING
	);
	PORT 
	(
			wrclk	: IN STD_LOGIC ;
			rdempty	: OUT STD_LOGIC ;
			rdreq	: IN STD_LOGIC ;
			wrusedw	: OUT STD_LOGIC_VECTOR (6 DOWNTO 0);
			aclr	: IN STD_LOGIC ;
			rdclk	: IN STD_LOGIC ;
			q	: OUT STD_LOGIC_VECTOR (31 DOWNTO 0);
			wrreq	: IN STD_LOGIC ;
			data	: IN STD_LOGIC_VECTOR (31 DOWNTO 0)
	);
	END COMPONENT;

BEGIN
	rdempty    <= sub_wire0;
	wrusedw    <= sub_wire1(ceil_log2(DEPTH)-1 DOWNTO 0);
	q    <= sub_wire2(WIDTH-1 DOWNTO 0);

	dcfifo_component : dcfifo
	GENERIC MAP (
		intended_device_family => DEVICE_FAMILY,
		lpm_width => WIDTH,
		lpm_numwords => DEPTH,
		lpm_widthu => ceil_log2(DEPTH),
		clocks_are_synchronized => "FALSE",
		lpm_type => "dcfifo",
		lpm_showahead => "OFF",
		overflow_checking => "ON",
		underflow_checking => "ON",
		use_eab => "ON",
		add_ram_output_register => "ON"
	)
	PORT MAP (
		wrclk => wrclk,
		rdreq => rdreq,
		aclr => aclr,
		rdclk => rdclk,
		wrreq => wrreq,
		data => data,
		rdempty => sub_wire0,
		wrusedw => sub_wire1,
		q => sub_wire2
	);
END SYN;
