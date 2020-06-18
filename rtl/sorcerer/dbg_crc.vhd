-- Project Sorcerer - RISC-V core
--
-- CRC calculation for Debug & Trace data transfer to Host
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

entity dbg_crc is
	generic
	(
		LENGTH: natural range 8 to 32 := 8  -- 8, 16 or 32 supported values
	);
	port
	(
	  clk : in std_logic;
		reset : in std_logic;
		data: in std_logic;
		enable: in std_logic;
		shift: in std_logic;
		sclr: in std_logic;
		crc_out: out std_logic_vector(31 downto 0)
	);
end dbg_crc;

architecture behaviour of dbg_crc is
  constant INIT_VALUE: std_logic_vector(31 downto 0) := x"FFFFFFFF";

  signal crc, next_crc: std_logic_vector(31 downto 0);
begin
	crc8: if LENGTH = 8 generate
	-- CRC8 = x^8 + x^2 + x + 1	
	next_crc(0) <= crc(7) xor data;
	next_crc(1) <= crc(0) xor crc(7);
	next_crc(2) <= crc(1) xor crc(7);
	next_crc(3) <= crc(2);
	next_crc(4) <= crc(3); 
	next_crc(5) <= crc(4);
	next_crc(6) <= crc(5);
	next_crc(7) <= crc(6); 
	hibits1: for i in 8 to 31 generate
		next_crc(i) <= crc(i);
	end generate;	
	end generate crc8;
	
	crc16: if LENGTH = 16 generate
	-- CRC16 = x^16 + x^12 + x^5 + 1	
	next_crc(0) <= crc(15) xor data;
	next_crc(1) <= crc(0);
	next_crc(2) <= crc(1);
	next_crc(3) <= crc(2);
	next_crc(4) <= crc(3); 
	next_crc(5) <= crc(4) xor crc(15) xor data;
	next_crc(6) <= crc(5);
	next_crc(7) <= crc(6);
	next_crc(8) <= crc(7);
	next_crc(9) <= crc(8);
  next_crc(10) <= crc(9);	
	next_crc(11) <= crc(10);
	next_crc(12) <= crc(11)  xor crc(15) xor data;
	next_crc(13) <= crc(12);
	next_crc(14) <= crc(13);
	next_crc(15) <= crc(14);
	hibits2: for i in 16 to 31 generate
		next_crc(i) <= crc(i);
	end generate;	
	end generate crc16;	
	
	crc32: if LENGTH = 32 generate
	-- CRC32 = x^32 + x^26 + x^23 + x^22 + x^16 + x^12 + x^11 + x^10 + x^8 + x^7 + x^6 + x^3 + x + 1	
	next_crc(0) <= crc(31) xor data;
	next_crc(1) <= crc(0) xor crc(31) xor data;
	next_crc(2) <= crc(1);
	next_crc(3) <= crc(2) xor crc(31) xor data;
	next_crc(4) <= crc(3); 
	next_crc(5) <= crc(4);
	next_crc(6) <= crc(5) xor crc(31) xor data;
	next_crc(7) <= crc(6) xor crc(31) xor data;
	next_crc(8) <= crc(7) xor crc(31) xor data;
	next_crc(9) <= crc(8);
  next_crc(10) <= crc(9) xor crc(31) xor data;	
	next_crc(11) <= crc(10) xor crc(31) xor data;
	next_crc(12) <= crc(11) xor crc(31) xor data;
	next_crc(13) <= crc(12);
	next_crc(14) <= crc(13);
	next_crc(15) <= crc(14);
	next_crc(16) <= crc(15) xor crc(31) xor data;
	next_crc(17) <= crc(16);
	next_crc(18) <= crc(17);
	next_crc(19) <= crc(18);
	next_crc(20) <= crc(19); 
	next_crc(21) <= crc(20);
	next_crc(22) <= crc(21) xor crc(31) xor data;
	next_crc(23) <= crc(22) xor crc(31) xor data;
	next_crc(24) <= crc(23);
	next_crc(25) <= crc(24);
  next_crc(26) <= crc(25) xor crc(31) xor data;	
	next_crc(27) <= crc(26);
	next_crc(28) <= crc(27);
	next_crc(29) <= crc(28);
	next_crc(30) <= crc(29);
	next_crc(31) <= crc(30);	
	end generate crc32;
	
	process(reset, clk)
	begin
		if reset = '1' then
			crc <= INIT_VALUE;
		elsif rising_edge(clk) then
			if sclr = '1' then
				crc <= INIT_VALUE;
			elsif enable = '1' then	
			  crc <= next_crc;
			elsif shift = '1' then
				crc <= '0' & crc(31 downto 1);
			end if;
		end if;
	end process;
	
	crc_out <= crc;
end behaviour;
