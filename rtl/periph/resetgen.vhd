-- Project Sorcerer - RISC-V core
--
-- Reset Pulse Generator
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
use IEEE.STD_LOGIC_unsigned.all;

entity resetgen is
  generic
  (
    THROUGH: natural range 0 to 1 := 0;
    RESET_PULSE: natural := 500000000;
    RESET_CYCLE: natural := 10000000 
  );
  port
  (
    clk: in std_logic;
    reset_in: in std_logic := '0';
    dbg_reset_in: in std_logic := '0';
    reset: out std_logic;
    reset_n: out std_logic;

    dbg_reset: out std_logic;
  
    rst_cycl: out std_logic;
    rst_cycl_n: out std_logic  
  );
 end resetgen;

architecture behaviour of resetgen is
  signal prescale_cnt: std_logic_vector(2 downto 0);
  signal reset_cnt: std_logic_vector(31 downto 0);
  signal reset_cycl_cnt: std_logic_vector(31 downto 0); 
  signal rs: std_logic;
  signal rc: std_logic;

  signal res_del1, res_del2: std_logic;
begin
  process (reset_in, dbg_reset_in, clk)
  begin
    if reset_in = '1' or dbg_reset_in = '1' then
      rs <= '0';
      rc <= '0';
      reset_cnt <= (others => '0');
    elsif rising_edge(clk) then
      if reset_cnt = RESET_PULSE-1 then
		  rs <= '1';
      else
		  rs <= '0';
        reset_cnt <= reset_cnt + 1;
      end if;
      if reset_cycl_cnt = RESET_CYCLE-1 then
  	     rc <= '1';
        reset_cycl_cnt <= (others => '0');
      else
        rc <= '0';
        reset_cycl_cnt <= reset_cycl_cnt + 1;
      end if;
    end if;
--	  if reset_in = '1' then
--		  reset <= '1';
--		  reset_n <= '0';
--	  else
--      if reset_cnt = RESET_PULSE-1 then
--        reset <= '0';
--        reset_n <= '1';
--      else
--        reset <= '1';
--        reset_n <= '0';    	
--      end if;	
  end process;

  process(reset_in, clk)
  begin
    if reset_in = '1' then
      res_del1 <= '1';	
      res_del2 <= '1';
    elsif rising_edge(clk) then
      res_del2 <= res_del1;
      res_del1 <= '0';
    end if;
  end process;

  dbg_reset <= res_del2;

  through_no: if THROUGH = 0 generate
    reset <= not rs;
    reset_n <= rs;  
    rst_cycl <= rc;
    rst_cycl_n <= not rc;
  end generate;

  through_yes: if THROUGH = 1 generate
    reset <= reset_in;
    reset_n <= not reset_in;  
    rst_cycl <= reset_in;
    rst_cycl_n <= not reset_in;
  end generate;  
end behaviour; 