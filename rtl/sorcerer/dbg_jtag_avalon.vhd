-- Project Sorcerer - RISC-V core
--
-- Virtual JTAG to Avalon MM Bus bridge
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
use work.iface.all;

entity dbg_jtag_avalon is
	generic
	(
		SERIAL_WIDTH: natural range 8 to 128 := WORD_SIZE;
		INT_BITS: natural range 1 to 32 := 1;
		SER_BITS: natural range 1 to 8 := 1;
		VERSION: natural range 0 to 15 := 1;
		PART_NUMBER: natural range 0 to 65535 := 1;
		MANUF_ID: natural range 0 to 2047 := 1
	);
  port
  (
	  clk: in std_logic;
		reset: in std_logic; 
		sys_reset: in std_logic;

		dbg_reset: out std_logic;  -- Debugger reset output
		dbg_ndreset: out std_logic;  -- Debugger non-halt reset	mode

		-- Avalon Bus Master Interface
		avm_dbgm_address: out std_logic_vector(BUS_ADDRESS_WIDTH-1 downto 0);
		avm_dbgm_read: out std_logic;
		avm_dbgm_readdata: in std_logic_vector(BUS_WIDTH-1 downto 0) := (others => '0');
		avm_dbgm_readdatavalid: in std_logic := '0';
		avm_dbgm_write: out std_logic;
		avm_dbgm_writedata: out std_logic_vector(BUS_WIDTH-1 downto 0);
		avm_dbgm_byteenable: out std_logic_vector(BUS_WIDTH/8 -1 downto 0);
		avm_dbgm_waitrequest: in std_logic := '0'		
	);	
end dbg_jtag_avalon;

architecture behaviour of dbg_jtag_avalon is
  -- Signals and registers declared for Virtual JTAG instance
  signal tck: std_logic;
	signal tdi: std_logic;
  signal capture_dr: std_logic;
	signal e1dr: std_logic;
	signal e2dr: std_logic;
	signal pdr: std_logic;
	signal shift_dr: std_logic;
	signal update_dr: std_logic;
	signal update_ir: std_logic;
	signal cir: std_logic;
	signal tms: std_logic;
  signal tdo: std_logic;
  signal IR: std_logic_vector(4 downto 0); -- IR command register		
	
  signal bus_address: std_logic_vector(BUS_ADDRESS_WIDTH-1 downto 0);
  signal bus_size: std_logic_vector(2 downto 0);
  signal bus_strobe: std_logic;
  signal bus_writedata: std_logic_vector(BUS_WIDTH-1 downto 0);
  signal bus_write: std_logic;
  signal bus_readdata: std_logic_vector(BUS_WIDTH-1 downto 0);
  signal bus_ready: std_logic;
begin
  bus_bridge1: dbg_avalon_bridge
	generic	map
	(
	  DATA_WIDTH => BUS_WIDTH,
		ADDRESS_WIDTH => BUS_ADDRESS_WIDTH
	)
  port map
  (
	  clk => clk,
		reset => reset,
		
		tck => tck,
		
		bus_address => bus_address, 
		bus_size => bus_size,
		bus_strobe => bus_strobe,
		bus_writedata => bus_writedata,
		bus_write => bus_write,
		bus_readdata => bus_readdata,
		bus_ready => bus_ready,
		
		avm_dbgm_address =>	avm_dbgm_address,
		avm_dbgm_read => avm_dbgm_read,
		avm_dbgm_readdata => 	avm_dbgm_readdata,
		avm_dbgm_readdatavalid => avm_dbgm_readdatavalid,
		avm_dbgm_write => avm_dbgm_write,
		avm_dbgm_writedata => avm_dbgm_writedata,
		avm_dbgm_byteenable => avm_dbgm_byteenable,
		avm_dbgm_waitrequest => avm_dbgm_waitrequest		
	);

  vjtag1: dbg_vjtag
	port map
	(
		ir_out => (others => '0'),
		tdo => tdo,
		ir_in => IR,
		tck => tck,
		tdi => tdi, 
		virtual_state_cdr => capture_dr,
		virtual_state_cir	=> cir,
		virtual_state_e1dr => e1dr,
		virtual_state_e2dr => e2dr,
		virtual_state_pdr	=> pdr,
		virtual_state_sdr	=> shift_dr,
		virtual_state_udr	=> update_dr,
		virtual_state_uir	=> update_ir	
	);
	
  transport1: dbg_jtag_transport
	generic	map
	(
	  DATA_WIDTH => BUS_WIDTH,
		ADDRESS_WIDTH => BUS_ADDRESS_WIDTH,
		SERIAL_WIDTH => SERIAL_WIDTH,
		INT_BITS => INT_BITS,
		SER_BITS => SER_BITS,
		VERSION => VERSION,
		PART_NUMBER => PART_NUMBER,
		MANUF_ID => MANUF_ID
	)
  port map
  (
	  clk => clk,
		reset => reset,

		dbg_reset => dbg_reset,
		dbg_ndreset => dbg_ndreset,
		
		bus_tck => tck,
		
		bus_address => bus_address,
		bus_size => bus_size,
		bus_strobe => bus_strobe,
		bus_writedata => bus_writedata,
		bus_write => bus_write,
		bus_readdata => bus_readdata,
		bus_ready => bus_ready,
		
    tck => tck,
	  tdi => tdi,
    capture_dr => capture_dr,
	  e1dr => e1dr,
	  e2dr => e2dr,
	  pdr => pdr,
	  shift_dr => shift_dr,
	  update_dr => update_dr,
	  update_ir => update_ir,
	  cir => cir,
	  tms => tms,
		IR => IR,
    tdo => tdo		
	);	
end behaviour;
