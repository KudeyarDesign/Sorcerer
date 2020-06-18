-- Project Sorcerer - RISC-V core
--
-- Debug Module with Avalon MM bus interface
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

library altera_mf;
use altera_mf.altera_mf_components.all;

entity dbg_module_avalon is
	generic
	(
		ROM_INIT_FILE: string:= ""
	);
	port
	(
	  clk: in std_logic;
		reset: in std_logic;
		ndreset: in std_logic := '0';
		
		halt0: out std_logic;
		dbg_irq0: out std_logic;

		dbg_set0: in std_logic := '0';
		
		-- Avalon Bus Slave
		avs_s1_address: in std_logic_vector(9 downto 0);
		avs_s1_writedata: in std_logic_vector(31 downto 0); 
		avs_s1_chipselect: in std_logic;
		avs_s1_read: in std_logic;
		avs_s1_write: in std_logic;	
		avs_s1_byteenable: in std_logic_vector(3 downto 0);
		avs_s1_readdata: out std_logic_vector(31 downto 0);
		avs_s1_waitrequest: out std_logic
--		avs_s1_readdatavalid: out std_logic
	);	
end dbg_module_avalon;

architecture behaviour of dbg_module_avalon is
  constant HARTID_BITS: natural := ceil_log2(HARTID_NUM);
	constant RAM_ADDRESS_BITS: natural := ceil_log2(DEBUG_RAM_SIZE);
	constant ROM_ADDRESS_BITS: natural := ceil_log2(DEBUG_ROM_SIZE);

	function addr_bits(address: std_logic_vector) return std_logic_vector is
	begin
		return address(avs_s1_address'high+2 downto 2);
	end function addr_bits;	
	
	constant CLEARDEBINT: std_logic_vector(avs_s1_address'range) := addr_bits(DEBUG_CLEARDEBINT);
	constant SETDEBINT: std_logic_vector(avs_s1_address'range) := addr_bits(DEBUG_SETDEBINT);
	constant CLEARHALTNOT: std_logic_vector(avs_s1_address'range) := addr_bits(DEBUG_CLEARHALTNOT);
	constant SETHALTNOT: std_logic_vector(avs_s1_address'range) := addr_bits(DEBUG_SETHALTNOT);
	constant GETHALTNOT: std_logic_vector(avs_s1_address'range) := addr_bits(DEBUG_GETHALTNOT);
	
  type dbgm_reg_type is record
    interrupt: std_logic_vector(HARTID_NUM-1 downto 0);
	  halt_notification: std_logic_vector(HARTID_NUM-1 downto 0);
		mem_request: std_logic;
		dbg_set0_del1, dbg_set0_del2: std_logic;
	end record;

	signal r, next_r: dbgm_reg_type;
	
	signal ram_cs: std_logic;
	signal ram_wr: std_logic;
	signal ram_readdata: std_logic_vector(31 downto 0);
	signal rom_cs: std_logic;
	signal rom_readdata: std_logic_vector(31 downto 0);	
	signal regs_readdata: std_logic_vector(31 downto 0);	
begin
	dbgm_ram1: altsyncram
	generic map 
	(
		clock_enable_input_a => "BYPASS",
		clock_enable_output_a => "BYPASS",
		intended_device_family => DEVICE_FAMILY,
		lpm_hint => "ENABLE_RUNTIME_MOD=NO",
		lpm_type => "altsyncram",
		numwords_a => DEBUG_RAM_SIZE/4,
		operation_mode => "SINGLE_PORT",
		outdata_aclr_a => "NONE",
		outdata_reg_a => "UNREGISTERED",
		power_up_uninitialized => "FALSE",
--		read_during_write_mode_port_a => "OLD_DATA",
		widthad_a => RAM_ADDRESS_BITS,
		width_a => 32,
		width_byteena_a => 4
	)
	port map 
	(
	  address_a => avs_s1_address(RAM_ADDRESS_BITS-1 downto 0),
		byteena_a => avs_s1_byteenable,
		clock0 => clk,
		data_a => avs_s1_writedata,
		wren_a => ram_wr,
		q_a => ram_readdata
	);

	dbgm_rom1 : altsyncram
	generic map 
	(
		address_aclr_a => "NONE",
		clock_enable_input_a => "BYPASS",
		clock_enable_output_a => "BYPASS",
		init_file => ROM_INIT_FILE,
		intended_device_family => DEVICE_FAMILY,
		lpm_hint => "ENABLE_RUNTIME_MOD=NO",
		lpm_type => "altsyncram",
		numwords_a => DEBUG_ROM_SIZE/4,
		operation_mode => "ROM",
		outdata_aclr_a => "NONE",
		outdata_reg_a => "UNREGISTERED",
		widthad_a => ROM_ADDRESS_BITS,
		width_a => 32,
		width_byteena_a => 4
	)
	port map 
	(
		address_a => avs_s1_address(ROM_ADDRESS_BITS-1 downto 0),
		clock0 => clk,
		q_a => rom_readdata
	);	

  ram_cs <= '1' when avs_s1_address >= addr_bits(DEBUG_RAM_START) and 
			               avs_s1_address < addr_bits(DEBUG_RAM_START+DEBUG_RAM_SIZE) 
    else '0';
	rom_cs <= '1' when avs_s1_address >= addr_bits(DEBUG_ROM_START) and 
			               avs_s1_address < addr_bits(DEBUG_ROM_START+DEBUG_ROM_SIZE) 
		else '0';	
	ram_wr <= avs_s1_write and ram_cs; 
	
	next_state_proc: process(r, 
	    avs_s1_address, avs_s1_chipselect, avs_s1_read, avs_s1_write, avs_s1_writedata,
	    rom_cs, ram_cs)
	  variable v: dbgm_reg_type;
		
	begin
		v := r;	
		v.mem_request := avs_s1_read and (ram_cs or rom_cs) and not r.mem_request;
		
		-- Avalon Bus operations
    regs_readdata <= (others => '0');		
		if avs_s1_chipselect = '1' then
			if avs_s1_write = '1' then
				case avs_s1_address is
					when CLEARDEBINT =>	 
					  if HARTID_NUM > 1 then
					    v.interrupt(CONV_INTEGER(avs_s1_writedata(HARTID_BITS-1 downto 0))) := '0';
						else
							v.interrupt(0) := '0';
						end if;	
					when SETDEBINT =>
					  if HARTID_NUM > 1 then
					    v.interrupt(CONV_INTEGER(avs_s1_writedata(HARTID_BITS-1 downto 0))) := '1';
						else
							v.interrupt(0) := '1';
						end if;					
					when CLEARHALTNOT => 
					  if HARTID_NUM > 1 then
					    v.halt_notification(CONV_INTEGER(avs_s1_writedata(HARTID_BITS-1 downto 0))) := '0';
						else
							v.halt_notification(0) := '0';
						end if;					
					when SETHALTNOT => 
					  if HARTID_NUM > 1 then
					    v.halt_notification(CONV_INTEGER(avs_s1_writedata(HARTID_BITS-1 downto 0))) := '1';
						else
							v.halt_notification(0) := '1';
						end if;					
					when others => null;
				end case;	
			end if;	
			if avs_s1_read = '1' then
				case avs_s1_address is
					when GETHALTNOT =>  regs_readdata(HARTID_NUM-1 downto 0) <= r.halt_notification;
					when others => null;
				end case;
			end if;	
		end if;		

		if r.dbg_set0_del1 = '1' and r.dbg_set0_del2 = '0' then
			v.interrupt(0) := '1';
		end if;	
		v.dbg_set0_del2 := r.dbg_set0_del1;
		v.dbg_set0_del1 := dbg_set0;
		
		next_r <= v;
	end process next_state_proc;	
	
	process(reset, ndreset, clk)
	begin
		if reset = '1' or ndreset = '1' then
			r.interrupt <= (others => '0');
			if ndreset = '0' then
			  r.halt_notification <= (others => '0');
			end if;	
			r.mem_request <= '0';	 
			r.dbg_set0_del1 <= '0';
			r.dbg_set0_del2 <= '0';
		elsif rising_edge(clk) then	
			r <= next_r;
		end if;	
	end process;
	
	halt0 <= r.halt_notification(0);
	dbg_irq0 <= r.interrupt(0);
	avs_s1_readdata <= ram_readdata when ram_cs = '1'
	  else rom_readdata when rom_cs = '1'
		else regs_readdata;
	avs_s1_waitrequest <= '1' when avs_s1_read = '1' and (ram_cs = '1' or rom_cs = '1') and r.mem_request = '0' 
	  else '0';
--	avs_s1_readdatavalid <= '0' when (ram_cs = '1' or rom_cs = '1') and r.mem_request = '0' 
--		else avs_s1_read;
end behaviour;
