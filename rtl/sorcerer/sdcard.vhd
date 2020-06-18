-- Project Sorcerer - RISC-V core
--
-- SD Card wrapper with Avalon MM interface
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

entity sdcard is 
	port
	(
	  clk: in std_logic;
		reset: in std_logic;

		int_cmd: out std_logic;		 -- Interrupt output 
		int_data: out std_logic;
		
		-- Avalon Bus Slave 
		avs_s1_address: in std_logic_vector(7 downto 0);
		avs_s1_writedata: in std_logic_vector(31 downto 0); 
		avs_s1_chipselect: in std_logic;
		avs_s1_read: in std_logic;
		avs_s1_write: in std_logic;
		avs_s1_byteenable: in std_logic_vector(3 downto 0);
		avs_s1_readdata: out std_logic_vector(31 downto 0);
		avs_s1_waitrequest: out std_logic;
		
		-- Avalon Bus Master
		avm_m1_address: out std_logic_vector(31 downto 0);
		avm_m1_writedata: out std_logic_vector(31 downto 0); 
		avm_m1_read: out std_logic;
		avm_m1_write: out std_logic;
		avm_m1_byteenable: out std_logic_vector(3 downto 0);
		avm_m1_readdata: in std_logic_vector(31 downto 0) := (others => '0');
		avm_m1_waitrequest: in std_logic := '0'; 
		avm_m1_readdatavalid: in std_logic := '0';
		avm_m1_burstcount: out std_logic_vector(4 downto 0);
		
		-- SD Card Interface
		sd_clk: out std_logic; 
		sd_cmd_out: out std_logic;
		sd_cmd_in: in std_logic;
		sd_cmd_oe: out std_logic;
		sd_data_out: out std_logic_vector(3 downto 0);
		sd_data_in: in std_logic_vector(3 downto 0);
		sd_data_oe: out std_logic
  );	
end sdcard;

architecture behaviour of sdcard is
  component sdc_controller is
	port						 
	(
    -- WISHBONE common
    wb_clk_i: in std_logic;
		wb_rst_i: in std_logic;	
	
    -- WISHBONE slave
		wb_dat_i: in std_logic_vector(31 downto 0);
		wb_dat_o: out std_logic_vector(31 downto 0);
    wb_adr_i: in std_logic_vector(7 downto 0);
		wb_sel_i: in std_logic_vector(3 downto 0);
		wb_we_i: in std_logic;
		wb_cyc_i: in std_logic;
		wb_stb_i: in std_logic;
		wb_ack_o: out std_logic;
		
    -- WISHBONE master
    m_wb_dat_o: out std_logic_vector(31 downto 0);
    m_wb_dat_i: in std_logic_vector(31 downto 0);
    m_wb_adr_o: out std_logic_vector(31 downto 0);
    m_wb_sel_o: out std_logic_vector(3 downto 0);
    m_wb_we_o: out std_logic;
    m_wb_cyc_o: out std_logic;
    m_wb_stb_o: out std_logic;
    m_wb_ack_i: in std_logic;
    m_wb_cti_o: out std_logic_vector(2 downto 0);
    m_wb_bte_o: out std_logic_vector(1 downto 0);
		
    -- SD BUS
    sd_cmd_dat_i: in std_logic;
    sd_cmd_out_o: out std_logic;
    sd_cmd_oe_o: out std_logic;
		
    sd_dat_dat_i: in std_logic_vector(3 downto 0);
    sd_dat_out_o: out std_logic_vector(3 downto 0);
    sd_dat_oe_o: out std_logic;
    sd_clk_o_pad: out std_logic;
    sd_clk_i_pad: in std_logic;
		
    int_cmd: out std_logic;
    int_data: out std_logic
  ); 
	end component;
	
	signal wb_ack_o: std_logic;
	signal m_wb_cyc_o: std_logic;
	signal m_wb_stb_o: std_logic;
	signal m_wb_we_o: std_logic;
	signal m_wb_ack_i: std_logic;
	signal m_wb_cti_o: std_logic_vector(2 downto 0);
	signal m_wb_bte_o: std_logic_vector(1 downto 0);
	signal m_burst: std_logic;
begin
  sdc: sdc_controller
	port map
	(
    wb_clk_i => clk,
		wb_rst_i => reset, 
		
		wb_dat_i => avs_s1_writedata,
		wb_dat_o => avs_s1_readdata,
    wb_adr_i => avs_s1_address,
		wb_sel_i => avs_s1_byteenable,
		wb_we_i => avs_s1_write,
		wb_cyc_i => avs_s1_chipselect,
		wb_stb_i => avs_s1_chipselect,
		wb_ack_o => wb_ack_o,
		
    m_wb_dat_o => avm_m1_writedata,
    m_wb_dat_i => avm_m1_readdata,
    m_wb_adr_o => avm_m1_address,
    m_wb_sel_o => avm_m1_byteenable,
    m_wb_we_o => m_wb_we_o,
    m_wb_cyc_o => m_wb_cyc_o,
    m_wb_stb_o => m_wb_stb_o,
    m_wb_ack_i => m_wb_ack_i,
    m_wb_cti_o => m_wb_cti_o,
    m_wb_bte_o => m_wb_bte_o,
		
    sd_cmd_dat_i => sd_cmd_in,
    sd_cmd_out_o => sd_cmd_out,
    sd_cmd_oe_o => sd_cmd_oe,
		
    sd_dat_dat_i => sd_data_in,
    sd_dat_out_o => sd_data_out,
    sd_dat_oe_o => sd_data_oe,
		
    sd_clk_o_pad => sd_clk,
    sd_clk_i_pad => clk,
		
    int_cmd => int_cmd,
    int_data => int_data
  ); 

	avs_s1_waitrequest <= avs_s1_chipselect and not wb_ack_o;	
	
	avm_m1_write <= m_wb_cyc_o and m_wb_stb_o and m_wb_we_o;
	avm_m1_read <= m_wb_cyc_o and m_wb_stb_o and not m_wb_we_o;
	m_burst <= '1' when m_wb_cti_o = "001" or  m_wb_cti_o = "010" else '0';
	avm_m1_burstcount <= 
	       CONV_STD_LOGIC_VECTOR(4, avm_m1_burstcount'length) when m_burst = '1' and  m_wb_bte_o = "01"
     else CONV_STD_LOGIC_VECTOR(8, avm_m1_burstcount'length) when m_burst = '1' and  m_wb_bte_o = "10"
		 else CONV_STD_LOGIC_VECTOR(16, avm_m1_burstcount'length) when m_burst = '1' and  m_wb_bte_o = "11"
		 else CONV_STD_LOGIC_VECTOR(1, avm_m1_burstcount'length);	 
	m_wb_ack_i <= m_wb_cyc_o and m_wb_stb_o and ((m_wb_we_o and not avm_m1_waitrequest) or (not m_wb_we_o and avm_m1_readdatavalid));
end behaviour;
