-- Project Sorcerer - RISC-V core
--
-- RAM Block, universal implementation
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
use work.utils.all;

entity ram_block is
	generic
	(
	  SIZE: natural := 1024;		-- size in bytes
		DATA_WIDTH: natural range 8 to 64 := 32  -- must be 2 ** N
	);
  port
  (
    clk : in std_logic;
	  rdaddress: in std_logic_vector(ceil_log2(SIZE*8/DATA_WIDTH)-1 downto 0);
	  wraddress: in std_logic_vector(ceil_log2(SIZE*8/DATA_WIDTH)-1 downto 0);
	  data : in std_logic_vector(DATA_WIDTH-1 downto 0);
	  wren: in std_logic;
	  byteen: in std_logic_vector(DATA_WIDTH/8-1 downto 0);
	 
	  q : out std_logic_vector(DATA_WIDTH-1 downto 0)
  );	
end ram_block;

architecture behaviour of ram_block is
  type word_type is array(0 to DATA_WIDTH/8-1) of std_logic_vector(7 downto 0);
  type ram_type is array (0 to SIZE-1) of word_type;
  signal ram: ram_type;	 
  attribute ram_style: string;
  attribute ram_style of ram : signal is "block"; -- "distributed" "Auto" 
 
  signal address_reg: std_logic_vector(rdaddress'range); 
  signal qarr: word_type;
begin
  column_ram_proc: process(clk)
  begin
	 if rising_edge(clk) then
	   if wren = '1' then
			  -- It's kind of trick, but loop doesn't work with Quartus synthesis
        if byteen(0) = '1' then
	        ram(CONV_INTEGER(wraddress))(0) <= data(7 downto 0); 
		    end if;
		    if DATA_WIDTH >= 16 then
          if byteen(1) = '1' then
	          ram(CONV_INTEGER(wraddress))(1) <= data(15 downto 8); 
		      end if;
        end if;
        if DATA_WIDTH >= 32 then		  
          if byteen(2) = '1' then
	          ram(CONV_INTEGER(wraddress))(2) <= data(23 downto 16); 
		      end if;		  
          if byteen(3) = '1' then
	         ram(CONV_INTEGER(wraddress))(3) <= data(31 downto 24); 
		      end if;
        end if;		  
       if DATA_WIDTH >= 64 then		  
          if byteen(4) = '1' then
	          ram(CONV_INTEGER(wraddress))(4) <= data(39 downto 32); 
		      end if;		  
          if byteen(5) = '1' then
	          ram(CONV_INTEGER(wraddress))(5) <= data(47 downto 40); 
		      end if;
          if byteen(6) = '1' then
	          ram(CONV_INTEGER(wraddress))(6) <= data(55 downto 48); 
				  end if;
          if byteen(7) = '1' then
	          ram(CONV_INTEGER(wraddress))(7) <= data(63 downto 56); 
		      end if;				 
        end if;		 
	    end if;
	 	  address_reg <= rdaddress;
    end if;	
  end process column_ram_proc;
  qarr <= ram(CONV_INTEGER(address_reg));  
  
  qout: for i in 0 to DATA_WIDTH/8-1 generate
    q(i*8+7 downto i*8) <= qarr(i);
  end generate;

end behaviour;
