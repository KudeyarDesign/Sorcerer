-- Project Sorcerer - RISC-V core
--
-- Interupt Controller (PLIC) for 8 external interrupt inputs & Avalon MM Bus slave
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

use work.iface.all;

entity plic8 is
	port
	(
	  clk: in std_logic;
		reset: in std_logic;
		
		irq0: in std_logic := '0';
		irq1: in std_logic := '0';
		irq2: in std_logic := '0';
		irq3: in std_logic := '0';
		irq4: in std_logic := '0';
		irq5: in std_logic := '0';
		irq6: in std_logic := '0';
		irq7: in std_logic := '0';
		
		intr: out std_logic;
		
		-- Avalon Bus Slave
		avs_s1_address: in std_logic_vector(2 downto 0);
		avs_s1_writedata: in std_logic_vector(31 downto 0); 
		avs_s1_chipselect: in std_logic;
		avs_s1_read: in std_logic;
		avs_s1_write: in std_logic;
		avs_s1_readdata: out std_logic_vector(31 downto 0)		
	);	
end plic8;

architecture behaviour of plic8 is
begin
	plic1: plic
	generic map
	(
		IRQ_NUM => 8
	)
	port map
	(
	  clk => clk,
		reset => reset,
		
		irq(0) => irq0,
		irq(1) => irq1,
		irq(2) => irq2,
		irq(3) => irq3,
		irq(4) => irq4,
		irq(5) => irq5,
		irq(6) => irq6,
		irq(7) => irq7,
		intr => intr,
		
		avs_s1_address => avs_s1_address,
		avs_s1_writedata => avs_s1_writedata,
		avs_s1_chipselect => avs_s1_chipselect,
		avs_s1_read => avs_s1_read, 
		avs_s1_write => avs_s1_write,
		avs_s1_readdata => avs_s1_readdata
	);	
	

end behaviour;
