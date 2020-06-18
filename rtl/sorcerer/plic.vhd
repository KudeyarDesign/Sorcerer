-- Project Sorcerer - RISC-V core
--
-- Interrupt Controller (PLIC)
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
use work.utils.all;

entity plic is
	generic
	(
		IRQ_NUM: natural range 1 to 32 := 8  -- Number of external interrupts
	);
	port
	(
	  clk: in std_logic;
		reset: in std_logic;
		
		irq: in std_logic_vector(IRQ_NUM-1 downto 0);
		intr: out std_logic;
		
		-- Avalon Bus Slave
		avs_s1_address: in std_logic_vector(2 downto 0);
		avs_s1_writedata: in std_logic_vector(31 downto 0); 
		avs_s1_chipselect: in std_logic;
		avs_s1_read: in std_logic;
		avs_s1_write: in std_logic;
		avs_s1_readdata: out std_logic_vector(31 downto 0)		
	);	
end plic;

architecture behaviour of plic is
  -- RTCC register addresses (offsets in bytes)
  constant IE_ADDRESS: natural := 0;			     -- Interrupt enable register
	constant TRIG_ADDRESS: natural := 4;		     -- Trigger mode register
	constant CLAIM_ADDRESS: natural := 8;		     -- Read interrupt ID
	constant COMPLETION_ADDRESS: natural := 12;	 -- Completion register
	
	constant IE_SADDR: std_logic_vector(avs_s1_address'range) := 
	  CONV_STD_LOGIC_VECTOR(IE_ADDRESS/4, avs_s1_address'length);	
	constant TRIG_SADDR: std_logic_vector(avs_s1_address'range) := 
	  CONV_STD_LOGIC_VECTOR(TRIG_ADDRESS/4, avs_s1_address'length);	
	constant CLAIM_SADDR: std_logic_vector(avs_s1_address'range) := 
	  CONV_STD_LOGIC_VECTOR(CLAIM_ADDRESS/4, avs_s1_address'length);
	constant COMPLETION_SADDR: std_logic_vector(avs_s1_address'range) := 
	  CONV_STD_LOGIC_VECTOR(COMPLETION_ADDRESS/4, avs_s1_address'length);	
	
  type state_type is (st_idle, st_claim, st_wait_completion);
  type reg_type is record
		state: state_type;
		irq: std_logic_vector(IRQ_NUM-1 downto 0);          -- Interrupt requests	
		irq_del: std_logic_vector(IRQ_NUM-1 downto 0);      -- Interrupt requests, delayed for 1 cycle
		TRIG: std_logic_vector(IRQ_NUM-1 downto 0);         -- Trigger mode register, '0' - level-triggered, '1' - front-triggered interrupt	
    EIP: std_logic_vector(IRQ_NUM-1 downto 0);          -- External interrupt pending register
		IE: std_logic_vector(IRQ_NUM-1 downto 0);           -- Interrupt enable register
		ID: std_logic_vector(ceil_log2(IRQ_NUM) downto 0);  -- Current interrupt ID
		claim: std_logic;
	end record;	
	
	signal r, next_r: reg_type;
begin
	next_state_proc: process(r, irq,
	    avs_s1_address, avs_s1_chipselect, avs_s1_read, avs_s1_write, avs_s1_writedata)
	  variable v: reg_type;
		variable id: std_logic_vector(r.ID'range);
		variable id_decode: std_logic_vector(2**r.ID'length-1 downto 0);

		procedure encodeId is
		begin
			id := (others => '0');
			for i in 0 to IRQ_NUM-1 loop
				if r.EIP(i) = '1' then
					id := CONV_STD_LOGIC_VECTOR(i + 1, r.ID'length); 
				end if;	
			end loop;	
		end procedure encodeId;		
	begin
		v := r;
		v.irq_del := r.irq;
		v.irq := irq;
		id_decode := decode(r.ID);
		v.EIP := r.EIP or 
		  (((r.irq and not r.TRIG) or (r.irq and not r.irq_del and r.TRIG))
		    and not id_decode(IRQ_NUM downto 1));
		
		if r.state = st_idle and r.EIP /= 0 then
			v.state := st_claim;
		end if;	
		
		-- Avalon Bus operations
    avs_s1_readdata <= (others => '0');		
		if avs_s1_chipselect = '1' then
			if avs_s1_write = '1' then
				case avs_s1_address is
					when IE_SADDR =>
					  v.IE := avs_s1_writedata(IRQ_NUM-1 downto 0);
					when TRIG_SADDR =>
					  v.TRIG := avs_s1_writedata(IRQ_NUM-1 downto 0);	
					when COMPLETION_SADDR =>
					  if r.state = st_wait_completion then
							v.state := st_idle;
						end if;
					when others => null;
				end case;	
			end if;	
			if avs_s1_read = '1' then
				case avs_s1_address is
					when IE_SADDR => 
					  avs_s1_readdata(IRQ_NUM-1 downto 0) <= r.IE;
					when TRIG_SADDR => 
					  avs_s1_readdata(IRQ_NUM-1 downto 0) <= r.TRIG;
					when CLAIM_SADDR =>
					  avs_s1_readdata(r.ID'range) <= r.ID; 
						v.claim := '1';
					when others => null;
				end case;
			end if;	
		end if;		
		
		encodeId;		
		if r.state = st_idle or v.state = st_claim then
			v.ID := id;			
		end if;	
		if r.claim = '1' and avs_s1_chipselect = '0' then
			v.claim := '0';
			v.EIP := r.EIP and not id_decode(IRQ_NUM downto 1);
			v.ID := (others => '0');
			v.state := st_wait_completion;			
		end if;	
		
		if r.state = st_claim then
			intr <= '1';
		else	
			intr <= '0';
		end if;	
		
		next_r <= v;
	end process next_state_proc;	

	process(reset, clk)
	begin
		if reset = '1' then
			r.state <= st_idle;
			r.irq <= (others => '0');
			r.irq_del <= (others => '0');
			r.IE <= (others => '0');
			r.EIP <= (others => '0');
			r.ID <= (others => '0');
			r.TRIG <= (others => '0');
			r.claim <= '0';
		elsif rising_edge(clk) then	
			r <= next_r;
		end if;	
	end process;
end behaviour;
