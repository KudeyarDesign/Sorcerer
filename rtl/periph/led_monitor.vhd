-- Project Sorcerer - RISC-V core
--
-- 7-Segnent LED interface
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

entity led_monitor is	
	generic
	(
	  REFRESH_PERIOD: natural := 35000000;
		BLINK_PERIOD: natural :=   30000000;
		READY_WAIT_TIMEOUT: natural := 100
	);
  port
  (											
	  clk: in std_logic;
		reset: in std_logic;	
	
  	data: in std_logic_vector(31 downto 0);
		valid: in std_logic := '1';
		hex0: out std_logic_vector(6 downto 0);
		hex1: out std_logic_vector(6 downto 0);
		hex2: out std_logic_vector(6 downto 0);
		hex3: out std_logic_vector(6 downto 0);
		hex4: out std_logic_vector(6 downto 0);
		hex5: out std_logic_vector(6 downto 0)
	);	
end led_monitor;

architecture behaviour of led_monitor is	
  function decode7seg(val: std_logic_vector(3 downto 0)) return std_logic_vector is
 	  variable led: std_logic_vector(6 downto 0);
	begin
		case val is
			when "0000" =>  led := "0111111";
			when "0001" =>  led := "0000110";
			when "0010" =>  led := "1011011";
			when "0011" =>  led := "1001111";
			when "0100" =>  led := "1100110";
			when "0101" =>  led := "1101101";
			when "0110" =>  led := "1111101";
			when "0111" =>  led := "0000111";
			when "1000" =>  led := "1111111";
			when "1001" =>  led := "1101111";
			when "1010" =>  led := "1110111";
			when "1011" =>  led := "1111100";
			when "1100" =>  led := "0111001";
			when "1101" =>  led := "1011110";
			when "1110" =>  led := "1111001";
			when "1111" =>  led := "1110001";
			when others =>  led := "1111111";
		end case;	
		return not led;
	end function decode7seg;	

	signal counter: std_logic_vector(31 downto 0);
	signal data_latch: std_logic_vector(31 downto 0);
	type state_type is (st_wait_valid, st_display, st_blink);
	signal state: state_type;	 
	signal turn_off: std_logic;
begin																						
	process(reset, clk)
	begin
		if reset = '1' then
			counter <= (others => '0');
			data_latch <= CONV_STD_LOGIC_VECTOR(REFRESH_PERIOD - 2, counter'length);
			turn_off <= '0';
			state <= st_wait_valid;
		elsif rising_edge(clk) then	
			case state is
				when st_wait_valid =>
				  if valid = '1' then
					  counter <= (others => '0');
						data_latch <= data;
						state <= st_display;
				  elsif counter = READY_WAIT_TIMEOUT - 1 then
					  counter <= (others => '0');
						turn_off <= '1';
						state <= st_blink;		
					else	
						counter <= counter + 1;
				  end if;
				when st_display =>
				  if counter = REFRESH_PERIOD-1 then
					  counter <= (others => '0');
						state <= st_wait_valid;
					else
						counter <= counter + 1;
					end if;
				when st_blink =>		
				  if valid = '1' then
					  counter <= (others => '0');
						data_latch <= data;				 
						turn_off <= '0';
						state <= st_display;				
				  elsif counter = BLINK_PERIOD/2 -1 then
					  counter <= (others => '0');
						data_latch <= data;
						turn_off <= not turn_off;
					else	
						counter <= counter + 1;
				  end if;					
			end case;		
		end if;	
	end process;	
	
	hex0 <= decode7seg(data_latch(3 downto 0)) when turn_off = '0' else (others => '1');
	hex1 <= decode7seg(data_latch(7 downto 4)) when turn_off = '0' else (others => '1');
	hex2 <= decode7seg(data_latch(11 downto 8)) when turn_off = '0' else (others => '1');
	hex3 <= decode7seg(data_latch(15 downto 12)) when turn_off = '0' else (others => '1');
	hex4 <= decode7seg(data_latch(19 downto 16)) when turn_off = '0' else (others => '1');
	hex5 <= decode7seg(data_latch(31 downto 28)) when turn_off = '0' else (others => '1');
end behaviour;
