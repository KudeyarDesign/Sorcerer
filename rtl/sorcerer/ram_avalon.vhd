-- Project Sorcerer - RISC-V core
--
-- RAM Module for Avalon MM Bus
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
use IEEE.STD_LOGIC_ARITH.all;
use work.config.all;
use work.utils.all;
use work.pipeline_iface.all;
use work.iface.all;

library altera_mf;
use altera_mf.altera_mf_components.all;

entity ram_avalon is
	generic
	(									 
	  DEVICE_FAMILY: string := "Cyclone V";
	  RAM_SIZE: natural range 1024 to 65536 := 16384;
		RAM_START_ADDRESS: std_logic_vector(PADDRESS_WIDTH-1 downto 0) := (others => '0');
		RAM_INIT_FILE: string
	);
	port
	(
	  clk : in std_logic;
		reset : in std_logic;  
		icache_in: in icache_fetch_in_type;
		icache_out: out icache_fetch_out_type;
		dcache_in: in dcache_mem_in_type;
		dcache_out: out dcache_mem_out_type;		
		
		-- Avalon Bus Master Interface
		avm_mem_address: out std_logic_vector(PADDRESS_WIDTH-1 downto 0);
		avm_mem_burstcount: out std_logic_vector(log2(imax(MAX_ICACHE_LINE_SIZE, MAX_DCACHE_LINE_SIZE)) downto 0);
		avm_mem_read: out std_logic;
		avm_mem_readdata: in std_logic_vector(BUS_WIDTH-1 downto 0);
		avm_mem_readdatavalid: in std_logic;
		avm_mem_write: out std_logic;
		avm_mem_writedata: out std_logic_vector(BUS_WIDTH-1 downto 0);
		avm_mem_byteenable: out std_logic_vector(BUS_WIDTH/8 -1 downto 0);
		avm_mem_waitrequest: in std_logic			
	);	
end ram_avalon;

architecture altera of ram_avalon is 
  constant ICACHE_RAM_ADDR_BITS: natural := ceil_log2(RAM_SIZE*8/ICACHE_FETCH_WIDTH);
	constant DCACHE_RAM_ADDR_BITS: natural := ceil_log2(RAM_SIZE*8/DCACHE_WIDTH);
	
	function is_ram_address(address: std_logic_vector(PADDRESS_WIDTH-1 downto 0)) return std_logic is
	begin
		if address(PADDRESS_WIDTH-1 downto ceil_log2(RAM_SIZE)) = 
			 RAM_START_ADDRESS(PADDRESS_WIDTH-1 downto ceil_log2(RAM_SIZE)) then
			return '1';
		else
			return '0';
		end if;	
	end function is_ram_address;	
	
  signal icache_q: std_logic_vector(ICACHE_FETCH_WIDTH-1 downto 0);
	signal dcache_q: std_logic_vector(DCACHE_WIDTH-1 downto 0);
	signal dcache_we: std_logic;

	signal icache_bus_in: icache_bus_in_type;
	signal icache_bus_out: icache_bus_out_type;
  signal dcache_bus_in: dcache_bus_in_type;
	signal dcache_bus_out: dcache_bus_out_type;

	signal icache_ram: std_logic;
	signal dcache_ram, next_dcache_ram: std_logic;
	signal icache_busy: std_logic;
	signal dcache_busy: std_logic;
begin	
	altsyncram_component : altsyncram
	generic map 
	(
		address_reg_b => "CLOCK0",
		byteena_reg_b => "CLOCK0",
		byte_size => 8,
		clock_enable_input_a => "BYPASS",
		clock_enable_input_b => "BYPASS",
		clock_enable_output_a => "BYPASS",
		clock_enable_output_b => "BYPASS",
		indata_reg_b => "CLOCK0",
		init_file => RAM_INIT_FILE,
		init_file_layout => "PORT_A",
		intended_device_family => DEVICE_FAMILY,
		lpm_type => "altsyncram",
		numwords_a => RAM_SIZE*8/ICACHE_FETCH_WIDTH,
		numwords_b => RAM_SIZE*8/DCACHE_WIDTH,
		operation_mode => "BIDIR_DUAL_PORT",
		outdata_aclr_a => "NONE",
		outdata_aclr_b => "NONE",
		outdata_reg_a => "UNREGISTERED",
		outdata_reg_b => "UNREGISTERED",
		power_up_uninitialized => "FALSE",
		read_during_write_mode_mixed_ports => "DONT_CARE",
--		read_during_write_mode_port_a => "OLD_DATA", -- "NEW_DATA_NO_NBE_READ",
--		read_during_write_mode_port_b => "NEW_DATA_NO_NBE_READ",
		widthad_a => ICACHE_RAM_ADDR_BITS,
		widthad_b => DCACHE_RAM_ADDR_BITS,
		width_a => ICACHE_FETCH_WIDTH,
		width_b => DCACHE_WIDTH,
		width_byteena_a => 1,
		width_byteena_b => DCACHE_WIDTH/8,
		wrcontrol_wraddress_reg_b => "CLOCK0"
	)
	port map 
	(
		clock0 => clk,
		wren_a => '0',
		address_b => dcache_in.next_vaddress(log2(ICACHE_FETCH_WIDTH/8)+ICACHE_RAM_ADDR_BITS-1 downto log2(ICACHE_FETCH_WIDTH/8)),
		byteena_b => dcache_in.next_byteenable,
		data_b => dcache_in.next_wr_data,
		wren_b => dcache_we,
		address_a => icache_in.next_pc(log2(ICACHE_FETCH_WIDTH/8)+ICACHE_RAM_ADDR_BITS-1 downto log2(ICACHE_FETCH_WIDTH/8)),
		addressstall_a => icache_in.address_stall,
		data_a => (others => '0'),
		q_a => icache_q,
		q_b => dcache_q
	);	
	
  avalon_mst1: avalon_master
  port map
  (
	  clk => clk,
		reset => reset,
		
		icache_in => icache_bus_out,
		icache_out => icache_bus_in,
		dcache_in => dcache_bus_out,
		dcache_out => dcache_bus_in, 
		mmu_in => (address => (others => 'X'), read => 'X'),
		mmu_out => open, 
		
		avm_mem_address => avm_mem_address,
		avm_mem_burstcount => avm_mem_burstcount,
		avm_mem_read => avm_mem_read,
		avm_mem_readdata => avm_mem_readdata,
		avm_mem_readdatavalid => avm_mem_readdatavalid,
		avm_mem_write => avm_mem_write,
		avm_mem_writedata => avm_mem_writedata,
		avm_mem_byteenable => avm_mem_byteenable,
		avm_mem_waitrequest => avm_mem_waitrequest
	);

	process(reset, clk)
	begin
		if reset = '1' then
			dcache_ram <= '0';
		elsif rising_edge(clk) then	
			if dcache_busy = '0' then
			  dcache_ram <= next_dcache_ram;
			end if;
		end if;
	end process;	
	
	icache_ram <= is_ram_address(icache_in.pc);
	icache_bus_out.address <= icache_in.pc;
	icache_bus_out.read <= icache_in.fetch and not icache_ram;

	icache_out.data <= icache_q when icache_ram = '1' else icache_bus_in.readdata;
	icache_out.ready <= icache_in.fetch when icache_ram = '1' else icache_in.fetch and icache_bus_in.readdatavalid;
	icache_busy <= '1' when icache_in.fetch = '1' and icache_ram = '0' and icache_bus_in.readdatavalid = '0' else '0';
	icache_out.busy	<= icache_busy;
	
	next_dcache_ram <= is_ram_address(dcache_in.next_vaddress);
	dcache_we <= dcache_in.next_wr and next_dcache_ram;
	dcache_bus_out.address <= dcache_in.vaddress;
	dcache_bus_out.read <= dcache_in.rd and not dcache_ram;	
	dcache_bus_out.write <= dcache_in.wr and not dcache_ram;
	dcache_bus_out.writedata <= dcache_in.wr_data;
	dcache_bus_out.byteenable <= dcache_in.byteenable;
	dcache_bus_out.locked <= '0';
	dcache_bus_out.burstcount <= CONV_STD_LOGIC_VECTOR(1, dcache_bus_out.burstcount'length);
	
	dcache_out.data <= dcache_q when dcache_ram = '1' else dcache_bus_in.readdata;
	dcache_out.ready <= dcache_in.rd when dcache_ram = '1' else dcache_in.rd and dcache_bus_in.readdatavalid;
	dcache_busy <= '1' when dcache_ram = '0' and 
	    ((dcache_in.rd = '1' and dcache_bus_in.readdatavalid = '0') or
		  (dcache_in.wr = '1' and dcache_bus_in.waitrequest = '1'))
	  else '0';
	dcache_out.busy <= dcache_busy;	
end altera;
