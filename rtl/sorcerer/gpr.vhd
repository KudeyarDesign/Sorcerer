-- Project Sorcerer - RISC-V core
--
-- General Purpose Reigster file (GPR)
-- Two different implementations - by using FPGA (or ASIC) triggers
-- or by using FPGA Memory Blocks
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
use work.utils.all;
use work.pipeline_iface.all;

entity gpr is
	generic
	(
		REGISTER_NUM: integer range 8 to 32 := 32
	);
	port
	(
		clk : in std_logic;
		reset : in std_logic;
		din: in gpr_in_type;
		dout: out gpr_out_type
	);	
end gpr;

architecture behaviour of gpr is 
  constant REG_BITS: integer := log2(REGISTER_NUM);

  signal s1_address: std_logic_vector(REG_BITS-1 downto 0);
	signal s2_address: std_logic_vector(REG_BITS-1 downto 0);
	
	type regs_array is array(0 to 31) of std_logic_vector(REGISTER_NUM-1 downto 0);
	signal r: regs_array;
	
	alias zero is r(0);
	alias ra is r(1);
	alias sp is r(2);
	alias gp is r(3);
	alias tp is r(4);
	alias t0 is r(5);
	alias t1 is r(6);
	alias t2 is r(7);	
	alias fp is r(8);
	alias s0 is r(8);
	alias s1 is r(9);
	alias a0 is r(10);
	alias a1 is r(11);
	alias a2 is r(12);
	alias a3 is r(13);
	alias a4 is r(14);
	alias a5 is r(15);
	alias a6 is r(16);
	alias a7 is r(17);
	alias s2 is r(18);
	alias s3 is r(19);
	alias s4 is r(20);
	alias s5 is r(21);
	alias s6 is r(22);
	alias s7 is r(23);
	alias s8 is r(24);
	alias s9 is r(25);
	alias s10 is r(26);
	alias s11 is r(27);
	alias t3 is r(28);
	alias t4 is r(29);
	alias t5 is r(30);
	alias t6 is r(31);
	
	alias x1 is r(1);
	alias x2 is r(2);
	alias x3 is r(3);
	alias x4 is r(4);
	alias x5 is r(5);
	alias x6 is r(6);
	alias x7 is r(7);
	alias x8 is r(8);
	alias x9 is r(9);
	alias x10 is r(10);
	alias x11 is r(11);
	alias x12 is r(12);
	alias x13 is r(13);
	alias x14 is r(14);
	alias x15 is r(15);
	alias x16 is r(16);
	alias x17 is r(17);
	alias x18 is r(18);
	alias x19 is r(19);
	alias x20 is r(20);
	alias x21 is r(21);
	alias x22 is r(22);
	alias x23 is r(23);
	alias x24 is r(24);
	alias x25 is r(25);
	alias x26 is r(26);
	alias x27 is r(27);
	alias x28 is r(28);
	alias x29 is r(29);
	alias x30 is r(30);
	alias x31 is r(31);	
begin
	
	process(reset, clk)
	begin
		if reset = '1' then
			for i in 0 to REGISTER_NUM-1 loop
				r(i) <= (others => '0');
			end loop;	
		elsif rising_edge(clk) then	
			if din.rd_address_stall = '0' then
				s1_address <= din.s1_address;
				s2_address <= din.s2_address;
			end if;	
			if din.d_we = '1' and din.d_address /= 0 then
				r(CONV_INTEGER(din.d_address)) <= din.d_data;
			end if;	
		end if;
	end process;
	
	dout.s1 <= r(CONV_INTEGER(s1_address));
	dout.s2 <= r(CONV_INTEGER(s2_address));
end behaviour;	

library altera_mf;
use altera_mf.altera_mf_components.all;

architecture altera of gpr is	
  constant REG_BITS: integer := log2(REGISTER_NUM);
  signal wrclk: std_logic;
	signal rs1: std_logic_vector(WORD_SIZE-1 downto 0);
	signal rs2: std_logic_vector(WORD_SIZE-1 downto 0);	
	signal d_data: std_logic_vector(WORD_SIZE-1 downto 0);
	signal s1_address: std_logic_vector(REG_BITS-1 downto 0);
	signal s2_address: std_logic_vector(REG_BITS-1 downto 0);
	signal d_address: std_logic_vector(REG_BITS-1 downto 0);
	signal d_we: std_logic;
begin											
	wrclk <= clk;

	process(reset, clk)
	begin
		if reset = '1' then
			d_address <= (others => '0');
			d_we <= '0';
		elsif rising_edge(clk) then	
			d_data <= din.d_data;
			d_address <= din.d_address;	
			if din.rd_address_stall = '0' then
			  s1_address <= din.s1_address;
			  s2_address <= din.s2_address;
			end if;	
			d_we <= din.d_we;
		end if;
	end process;	
	
	bank_s : altsyncram
	generic map 
	(										
		address_aclr_b => "NONE",
		address_reg_b => "CLOCK1",
		byte_size => 8,
		clock_enable_input_a => "BYPASS",
		clock_enable_input_b => "BYPASS",
		clock_enable_output_b => "BYPASS",
		intended_device_family => DEVICE_FAMILY,
		init_file => "mif/gpr.hex",
		lpm_type => "altsyncram",
		numwords_a => REGISTER_NUM,
		numwords_b => REGISTER_NUM,
		operation_mode => "DUAL_PORT",
		outdata_aclr_b => "NONE",
		outdata_reg_b => "UNREGISTERED",
		power_up_uninitialized => "FALSE",
		ram_block_type => "AUTO",
		read_during_write_mode_mixed_ports => "DONT_CARE",
		widthad_a => REG_BITS,
		widthad_b => REG_BITS,
		width_a => WORD_SIZE,
		width_b => WORD_SIZE,
		width_byteena_a => WORD_SIZE/8
	)
	port map 
	(														 
	  address_a => din.d_address,
		clock0 => wrclk,
		data_a => din.d_data,
		wren_a => din.d_we,
		byteena_a => (others => '1'),	
		addressstall_b => din.rd_address_stall,
		address_b => din.s1_address(REG_BITS-1 downto 0),
		clock1 => clk,
		q_b => rs1
	);	
	
	bank_t : altsyncram
	generic map 
	(										
		address_aclr_b => "NONE",
		address_reg_b => "CLOCK1",
		byte_size => 8,
		clock_enable_input_a => "BYPASS",
		clock_enable_input_b => "BYPASS",
		clock_enable_output_b => "BYPASS",
		intended_device_family => DEVICE_FAMILY,
		init_file => "mif/gpr.hex",
		lpm_type => "altsyncram",
		numwords_a => REGISTER_NUM,
		numwords_b => REGISTER_NUM,
		operation_mode => "DUAL_PORT",
		outdata_aclr_b => "NONE",
		outdata_reg_b => "UNREGISTERED",
		power_up_uninitialized => "FALSE",
		ram_block_type => "AUTO",
		read_during_write_mode_mixed_ports => "DONT_CARE",
		widthad_a => REG_BITS,
		widthad_b => REG_BITS,
		width_a => WORD_SIZE,
		width_b => WORD_SIZE,
		width_byteena_a => WORD_SIZE/8
	)
	port map 
	(														 
	  address_a => din.d_address,
		clock0 => wrclk,
		data_a => din.d_data,
		wren_a => din.d_we,
		byteena_a => (others => '1'),	
		addressstall_b => din.rd_address_stall,
		address_b => din.s2_address(REG_BITS-1 downto 0),
		clock1 => clk,
		q_b => rs2
	);	
	
	-- Output bypass read during write
	dout.s1 <= d_data when d_we = '1' and d_address = s1_address else rs1;
	dout.s2 <= d_data when d_we = '1' and d_address = s2_address else rs2;	
	
end altera;
