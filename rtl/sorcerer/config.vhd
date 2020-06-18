-- Project Sorcerer - RISC-V core
--
-- Processor Configuration constants
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

package config is	
	constant WORD_SIZE: natural:= 32;
	constant VADDRESS_WIDTH: natural:= 32;	-- virtual address
	constant PADDRESS_WIDTH: natural:= 32;	-- physical address
	constant ADDRESS_WIDTH: natural:= 32;	  -- physical address (bus)

  constant INSN_SLICE_NUM: natural range 2 to 4 := 2; -- Maximum number if 16-bit slices in instruction	
	
  constant START_ADDRESS: std_logic_vector(VADDRESS_WIDTH-1 downto 0) := x"80000000";	
	
  constant DEBUG_START: std_logic_vector(PADDRESS_WIDTH-1 downto 0) := x"00000100";
  constant DEBUG_ROM_START: std_logic_vector(PADDRESS_WIDTH-1 downto 0) := x"00000800";
  constant DEBUG_ROM_RESUME: std_logic_vector(PADDRESS_WIDTH-1 downto 0) := (DEBUG_ROM_START + 4);
  constant DEBUG_ROM_EXCEPTION: std_logic_vector(PADDRESS_WIDTH-1 downto 0) := (DEBUG_ROM_START + 8);
	constant DEBUG_ROM_SIZE: natural := 512;
  constant DEBUG_ROM_END: std_logic_vector(PADDRESS_WIDTH-1 downto 0) := (DEBUG_ROM_START + DEBUG_ROM_SIZE - 1);
  constant DEBUG_RAM_START: std_logic_vector(PADDRESS_WIDTH-1 downto 0) := x"00000400";
  constant DEBUG_RAM_SIZE: natural := 64;
  constant DEBUG_RAM_END: std_logic_vector(PADDRESS_WIDTH-1 downto 0) := (DEBUG_RAM_START + DEBUG_RAM_SIZE);
  constant DEBUG_END: std_logic_vector(PADDRESS_WIDTH-1 downto 0) := x"00000FFF";
  constant DEBUG_CLEARDEBINT: std_logic_vector(PADDRESS_WIDTH-1 downto 0) := x"00000100";
  constant DEBUG_SETDEBINT: std_logic_vector(PADDRESS_WIDTH-1 downto 0) := x"00000104";	
  constant DEBUG_CLEARHALTNOT: std_logic_vector(PADDRESS_WIDTH-1 downto 0) := x"00000108";	
  constant DEBUG_SETHALTNOT: std_logic_vector(PADDRESS_WIDTH-1 downto 0) := x"0000010C";
	constant DEBUG_GETHALTNOT: std_logic_vector(PADDRESS_WIDTH-1 downto 0) := x"0000010C";
  constant DEBUG_SIZE: natural := CONV_INTEGER(DEBUG_END - DEBUG_START + 1);
	
  constant EXT_IO_BASE: std_logic_vector(VADDRESS_WIDTH-1 downto 0) := x"40000000";
  constant RTCC_IO_BASE: std_logic_vector(VADDRESS_WIDTH-1 downto 0) := EXT_IO_BASE;	
	constant PLIC_IO_BASE: std_logic_vector(VADDRESS_WIDTH-1 downto 0) := EXT_IO_BASE + 64;
  constant CONSOLE_IO_BASE: std_logic_vector(VADDRESS_WIDTH-1 downto 0) := EXT_IO_BASE + 128;	
	
	constant DEVICE_FAMILY: string := "Cyclone V";
	
	constant BUS_ADDRESS_WIDTH: natural:= 32;
	constant BUS_WIDTH: natural := 32; 	
	constant BUS_TIMEOUT: natural range 0 to 255 := 0;  -- External Bus Timeout, 0 - no timeout control
	
	constant HARTID_NUM: natural range 1 to 10 := 1; -- Number of hart's

	constant BHT_SIZE: natural range 128 to 65536 := 4086;
	constant BTB_SIZE: natural range 1 to 65536 := 256;
	constant BTB_ASSOCIATIVITY: natural range 1 to 4 := 2;
	constant RAS_DEPTH: natural range 1 to 256 := 8;
	
	constant ICACHE_LINE_SIZE: natural range 4 to 64 := 8;  -- In memory words
	
	constant ICACHE_FETCH_WIDTH: natural := 32;
	constant ICACHE_SIZE: natural := 512;  -- in bytes
	constant MAX_ICACHE_LINE_SIZE: natural := 16;  -- in memory words (with ICACHE_FETCH_WIDTH)
	constant ICACHE_ASSOCIATIVITY: natural range 1 to 16 := 2;
	
	constant DCACHE_LINE_SIZE: natural range 4 to 64 := 8;  -- In memory words
	
	constant DCACHE_WIDTH: natural := 32;
	constant DCACHE_SIZE: natural := 512;  -- in bytes
	constant MAX_DCACHE_LINE_SIZE: natural := 16;  -- in memory words (with DATA_WIDTH)	
	constant DCACHE_ASSOCIATIVITY: natural range 1 to 16 := 2;
	
	constant TRACE_BURST_LEN: natural range 4 to 64 := 8;
	constant TRACE_BUS_WIDTH: natural range 16 to 256 := BUS_WIDTH;
	
	constant PAGE_LEVELS: natural range 2 to 3 := 2;
	constant PAGE_IDX_BITS: natural := 12;
	constant ASID_BITS: natural := 7;
	constant VPN_BITS: natural := VADDRESS_WIDTH - PAGE_IDX_BITS;
	constant PPN_BITS: natural := PADDRESS_WIDTH - PAGE_IDX_BITS;
	
	constant TLB_SIZE: natural range 16 to 64 := 16;
	constant TLB_DCACHE_SIZE: natural range 2 to 8 := 4;
	
	constant hasCache: boolean := true;
	constant hasMMU: boolean := false;	 
	constant hasFixedMappingMMU: boolean := false;
	constant hasDebugger: boolean := false;

	constant isVirtualICache: boolean := false;
	constant isDebugASID: boolean := false;
	constant isVariablePageSize: boolean := false;
	constant isTLBCacheUseASID: boolean := false;

	constant isCacheAutoInit: boolean := true;
	constant isFastROM: boolean := true;
	
	constant ENABLE_DISASSEMBLER: boolean := true;
end package config;

package body config is

end package body config;
