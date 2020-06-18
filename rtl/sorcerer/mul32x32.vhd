-- Project Sorcerer - RISC-V core
--
-- 32 x 32 Multiplier, 2-cycle pipelined operation
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
use work.pipeline_iface.all;
use work.iface.all;

entity mul32x32 is
	port
	(	 
		clk : in std_logic;
		reset : in std_logic;	  
	  din: in mul_in_type;
		dout: out mul_out_type
	);	
end mul32x32;

architecture behaviour of mul32x32 is
  signal signa00: std_logic;
	signal signb00: std_logic;
  signal signa01: std_logic;
	signal signb01: std_logic;	
  signal signa10: std_logic;
	signal signb10: std_logic;	
  signal signa11: std_logic;
	signal signb11: std_logic;	

  signal result00: std_logic_vector(31 downto 0);
  signal result01: std_logic_vector(31 downto 0);
	signal result10: std_logic_vector(31 downto 0);
	signal result11: std_logic_vector(31 downto 0);
	
	signal sign01: std_logic_vector(15 downto 0);
	signal sign10: std_logic_vector(15 downto 0);
	
  signal reg00: std_logic_vector(31 downto 0);
  signal reg01: std_logic_vector(31 downto 0);
	signal reg10: std_logic_vector(31 downto 0);
	signal reg11: std_logic_vector(31 downto 0);
	signal rsign1: std_logic;	
	signal rsign2: std_logic;
begin
	signa00 <= '0';
	signa01 <= din.sign1 or din.sign2;
	signa10 <= '0';
	signa11 <= din.sign1 or din.sign2;
	signb00 <= '0';
	signb01 <= '0'; 
	signb10 <= din.sign1 and din.sign2;
	signb11 <= din.sign1 and din.sign2;			
	
  mul00: mul16x16
	port map
	(
		dataa_0 => din.rs1(15 downto 0),
		datab_0	=> din.rs2(15 downto 0),
		signa	=> signa00,
		signb	=> signb00,
		result => result00	
	);
	
  mul01: mul16x16
	port map
	(
		dataa_0 => din.rs1(31 downto 16),
		datab_0	=> din.rs2(15 downto 0),
		signa	=> signa01,
		signb	=> signb01,
		result => result01	
	);
	
  mul10: mul16x16
	port map
	(
		dataa_0 => din.rs1(15 downto 0),
		datab_0	=> din.rs2(31 downto 16),
		signa	=> signa10,
		signb	=> signb10,
		result => result10	
	);
	
  mul11: mul16x16
	port map
	(
		dataa_0 => din.rs1(31 downto 16),
		datab_0	=> din.rs2(31 downto 16),
		signa	=> signa11,
		signb	=> signb11,
		result => result11	
	);	

	sign01 <= x"FFFF" when rsign1 = '1' and reg01(31) = '1' else x"0000";
  sign10 <= x"FFFF" when rsign2 = '1' and reg10(31) = '1' else x"0000";		
		
	process(reset, clk)
	begin
		if reset = '1' then
			null;
		elsif rising_edge(clk) then	
			reg00 <= result00;
			reg01 <= result01;
			reg10 <= result10;
			reg11 <= result11;
			rsign1 <= din.sign1 or din.sign2;
			rsign2 <= din.sign1 and din.sign2;
		end if;
	end process;

	dout.result(15 downto 0) <= reg00(15 downto 0);
	dout.result(63 downto 16) <= 
	  (reg11 & x"0000") +
	  (sign01 & reg01) + 
		(sign10 & reg10) + 
		(x"00000000" & reg00(31 downto 16));
end behaviour;
