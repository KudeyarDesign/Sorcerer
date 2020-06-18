-- Project Sorcerer - RISC-V core
--
-- Branch History Table
-- gshare + 2-bit saturation counter table 
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

entity bht is
	generic
	(
		ENABLE_C: natural range 0 to 1 := 0      -- Compressed extension
	);
	port
	(
		clk : in std_logic;
		reset : in std_logic;
		
		din: in bht_in_type;
		dout: out bht_data_type;
		upd_in: in bht_update_type
	);	
end bht;

architecture behaviour of bht is
  constant HISTORY_BITS: natural := ceil_log2(BHT_SIZE);  -- gshare bit width

  signal history, next_history: std_logic_vector(HISTORY_BITS-1 downto 0);
	signal ram_rd_address: std_logic_vector(HISTORY_BITS-1 downto 0);
	signal ram_rd_data: std_logic_vector(1 downto 0);
	signal ram_wr: std_logic;
	signal ram_wr_address: std_logic_vector(HISTORY_BITS-1 downto 0);
	signal ram_wr_data: std_logic_vector(1 downto 0);
	
	type bht_ram_type is array(0 to BHT_SIZE-1) of std_logic_vector(1 downto 0);
	signal ram: bht_ram_type := (others => (others => '0'));
	
	function hash(address: std_logic_vector(VADDRESS_WIDTH-1 downto 0); 
	  history: std_logic_vector(HISTORY_BITS-1 downto 0)) return std_logic_vector is
	begin
		if ENABLE_C = 1 then
		  return address(HISTORY_BITS downto 1) xor history;
		else
			return address(HISTORY_BITS+1 downto 2) xor history;
		end if;	
	end function hash;
	
	function next_state(state: bht_state_type; taken: std_logic) 
	  return std_logic_vector is
	begin																											 
		case state is
	    when BR_STRONG_NOT_TAKEN =>	 
			  if taken = '1' then
					return BR_WEAK_NOT_TAKEN;
				end if;
	    when BR_WEAK_NOT_TAKEN =>	 
			  if taken = '1' then
					return BR_STRONG_TAKEN;
				else	
					return BR_STRONG_NOT_TAKEN;
				end if;
	    when BR_WEAK_TAKEN =>
			  if taken = '1' then
					return BR_STRONG_TAKEN;
				else	
					return BR_STRONG_NOT_TAKEN;
				end if;
	    when BR_STRONG_TAKEN =>	
			  if taken = '0' then
					return BR_WEAK_TAKEN;
				end if;
			when others => null;
		end case;	
		return state;
	end function next_state;	
begin
	ram_rd_address <= hash(din.next_pc, history);
	ram_wr <= upd_in.update;
	ram_wr_address <= hash(upd_in.pc, history);
	
	bht_ram_proc: process(clk)
	begin
		if rising_edge(clk) then
			if ram_wr = '1' then
				ram(CONV_INTEGER(ram_wr_address)) <= ram_wr_data;
			end if;
			ram_rd_data <= ram(CONV_INTEGER(ram_rd_address));
		end if;
	end process bht_ram_proc;
	
	next_state_proc: process(history, ram_rd_data, upd_in)
	  variable v: std_logic_vector(HISTORY_BITS-1 downto 0);
		variable taken: std_logic;
	begin
		v := history;	 
		taken := ram_rd_data(0);
		if din.update = '1' then
			v := taken & history(history'high downto 1);
		end if;
		
		if upd_in.mispredict = '1' then
			v := upd_in.branch_taken & upd_in.prediction.history(upd_in.prediction.history'high downto 1);
		end if;
		ram_wr_data <= next_state(upd_in.prediction.state, upd_in.branch_taken);
		next_history <= v;
	end process next_state_proc;	
	
	dout.state <= ram_rd_data;
	dout.history <= history;
	
	process(reset, clk)
	begin
		if reset = '1' then
			history <= (others => '0');
		elsif rising_edge(clk) then	
			history <= next_history;
		end if;
	end process;	
end behaviour;
