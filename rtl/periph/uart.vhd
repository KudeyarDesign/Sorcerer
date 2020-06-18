-- Project Sorcerer - RISC-V core
--
-- Simple UART for Avalon MM Bus
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

entity uart is
	port
	(
	  clk: in std_logic;
		reset: in std_logic;

		txd: out std_logic;
		rxd: in std_logic;
		
		intr: out std_logic;															 -- Interrupt output
		
		-- Avalon Bus Slave 
		avs_s1_address: in std_logic_vector(2 downto 0);
		avs_s1_writedata: in std_logic_vector(31 downto 0); 
		avs_s1_chipselect: in std_logic;
		avs_s1_read: in std_logic;
		avs_s1_write: in std_logic;
		avs_s1_readdata: out std_logic_vector(31 downto 0)
  );		
end uart;

architecture behaviour of uart is
  type rx_state_type is (st_idle, st_startbit, st_data, st_parity, st_stopbit);
  type tx_state_type is (st_idle, st_data, st_parity, st_stopbit);
	type intr_type is record
		error: std_logic;
		rxfull: std_logic;	
		txempty: std_logic;
	end record;	
  type reg_type is record
		rx_state: rx_state_type;
		tx_state: tx_state_type;
		
		rxen: std_logic;		  -- receiver enabled
		txen: std_logic;			-- transmitter enabled
		
		dready: std_logic;	-- data ready
    rsempty: std_logic;	-- receiver shift register empty (internal)
    tsempty: std_logic;	-- transmitter shift register empty
    thempty: std_logic;	-- transmitter hold register empty		
		ovf: std_logic;	    -- receiver overflow	
    break: std_logic;	  -- break detected
    parerr: std_logic;	-- parity error
    frame:  std_logic;	-- framing error		
		
    rxdb: std_logic_vector(1 downto 0);  -- rx delay
    dpar: std_logic;	-- rx data parity
		
    rhold: std_logic_vector(7 downto 0);
    rshift: std_logic_vector(7 downto 0);
    tshift: std_logic_vector(10 downto 0);
    thold: std_logic_vector(7 downto 0);
    rxf:  std_logic_vector(7 downto 0); --  rx data filtering buffer		
		
    tpar:  std_logic;	-- tx data parity
		
		prescaler: std_logic_vector(19 downto 0);
    rxclk: std_logic_vector(2 downto 0); -- rx clock divider
		rxtick: std_logic;	-- rx clock
    txclk: std_logic_vector(2 downto 0);  -- tx clock divider
    txtick: std_logic;	-- tx clock		

    tick: std_logic;	-- clock		
		
		brate: std_logic_vector(19 downto 0);	 -- baud rate
		pen: std_logic;   -- '0' - no parity, '1' - enable parity
		pev: std_logic;	 -- '1' - even parity

		read_buf: std_logic;
		
		ie: intr_type;
		iff: intr_type; 
	end record;
	
	signal r, next_r: reg_type;
begin	
	next_state_proc: process(r,	
	    avs_s1_chipselect, avs_s1_read, avs_s1_write, avs_s1_writedata)
	  variable v: reg_type;		 
		variable rxclk, txclk : std_logic_vector(2 downto 0);
		variable vrxd: std_logic;
		variable ctsn: std_logic;	
		variable vprescaler: std_logic_vector(r.prescaler'high downto 0);
	begin
		v := r;
    v.txtick := '0'; 
		v.rxtick := '0'; 
		v.tick := '0';
    v.rxdb(1) := r.rxdb(0);		
		
    -- prescaler
    vprescaler := r.prescaler - 1;
    if (r.rxen or r.txen) = '1' then
      v.prescaler := vprescaler;
      v.tick := vprescaler(vprescaler'high) and not r.prescaler(vprescaler'high);
      if v.tick = '1' then 
        v.prescaler := r.brate; 
      end if;
    end if;		
		
    -- tx clock
    txclk := r.txclk + 1;
    if r.tick = '1' then
      v.txclk := txclk;
      v.txtick := r.txclk(2) and not txclk(2);
    end if;

    -- rx clock
    rxclk := r.rxclk + 1;
    if r.tick = '1' then
      v.rxclk := rxclk;
      v.rxtick := r.rxclk(2) and not rxclk(2);
    end if;

    -- filter rx data
    v.rxf := r.rxf(6 downto 0) & rxd;
    if ((r.rxf(7) & r.rxf(7) & r.rxf(7) & r.rxf(7) & r.rxf(7) & r.rxf(7) &
         r.rxf(7)) = r.rxf(6 downto 0)) then 
      v.rxdb(0) := r.rxf(7); 
    end if;		
		
		vrxd := r.rxdb(0);
		
    -- transmitter operation
    case r.tx_state is
      when st_idle =>	
        if (r.txtick = '1') then 
          v.tsempty := '1'; 
        end if;
        if (r.txen and (not r.thempty) and r.txtick) = '1' then
	        v.tshift := "10" & r.thold & '0';  
	        v.tpar := not r.pev; 
					v.iff.txempty := r.iff.txempty or r.ie.txempty; 
					v.thempty := '1';
	        v.tsempty := '0';
					v.txclk := "00" & r.tick; 
					v.txtick := '0';
					v.tx_state := st_data;
        end if;
      when st_data =>	-- transmitt data frame
        if r.txtick = '1' then
	        v.tpar := r.tpar xor r.tshift(1);
	        v.tshift := '1' & r.tshift(10 downto 1);
          if r.tshift(10 downto 1) = "1111111110" then
	          if r.pen = '1' then
	            v.tshift(0) := r.tpar; 
							v.tx_state := st_parity;
	          else
	            v.tshift(0) := '1'; 
							v.tx_state := st_stopbit;
	          end if;
	        end if;
        end if;
      when st_parity =>	-- transmitt parity bit
        if r.txtick = '1' then
	        v.tshift := '1' & r.tshift(10 downto 1); 
					v.tx_state := st_stopbit;
        end if;
      when st_stopbit =>	-- transmitt stop bit
        if r.txtick = '1' then
	        v.tshift := '1' & r.tshift(10 downto 1); 
					v.tx_state := st_idle;
        end if;
    end case;		

    -- receiver operation
    case r.rx_state is
      when st_idle =>	-- wait for start bit
        if ((not r.rsempty) and not r.dready) = '1' then
	        v.rhold := r.rshift; 
					v.rsempty := '1'; 
					v.dready := '1';
					v.iff.rxfull := r.iff.rxfull or r.ie.rxfull;
        end if;
        if (r.rxen and r.rxdb(1) and (not vrxd)) = '1' then
					v.rshift := (others => '1'); 
					v.rxclk := "100";
	        if v.rsempty = '0' then 
            v.ovf := '1';  
						v.iff.error := r.iff.error or r.ie.error;
          end if;
	        v.rsempty := '0'; 
					v.rxtick := '0'; 
	        v.rx_state := st_startbit; 					
        end if;
      when st_startbit =>	-- check validity of start bit
        if r.rxtick = '1' then
	        if vrxd = '0' then 
	          v.rshift := vrxd & r.rshift(7 downto 1); 
	          v.dpar := not r.pev;
						v.rx_state := st_data;
	        else
	          v.rx_state := st_idle;
          end if;
        end if;
      when st_data =>	-- receive data frame
        if r.rxtick = '1' then
	        v.dpar := r.dpar xor vrxd;
	        v.rshift := vrxd & r.rshift(7 downto 1); 
	        if r.rshift(0) = '0' then
	          if r.pen = '1' then 
              v.rx_state := st_parity;
	          else
							v.dpar := '0';
	            v.rx_state := st_stopbit; 
	          end if;
	        end if;
        end if;
      when st_parity =>	-- receive parity bit
        if r.rxtick = '1' then
	        v.dpar := r.dpar xor vrxd; 
	        v.rx_state := st_stopbit;
        end if;
      when st_stopbit =>	-- receive stop bit
        if r.rxtick = '1' then
	        if vrxd = '1' then
	          v.parerr := r.dpar; 
						v.rsempty := r.dpar;
	          if v.dready = '0' then
	            v.rhold := r.rshift; 
							v.rsempty := '1'; 	
							v.dready := not r.dpar;					 
							if r.dpar = '0' then
							  v.iff.rxfull := r.iff.rxfull or r.ie.rxfull;
							else
								v.iff.error := r.iff.error or r.ie.error;
							end if;	
	          end if;
	        else
	          if r.rshift = "00000000" then
	            v.break := '1'; 		 -- break	
							v.iff.error := r.iff.error or r.ie.error;
	          else
	            v.frame := '1'; 		 -- framing error		 
							v.iff.error := r.iff.error or r.ie.error;
	          end if;
	          v.rsempty := '1';
	        end if;
          v.rx_state := st_idle;
        end if;
    end case;		
		
		-- Avalon Bus operations 
		avs_s1_readdata <= (others => '0');
		if avs_s1_chipselect = '1' then
			if avs_s1_write = '1' then
				case avs_s1_address is
					when "000" =>	 -- Receiver Status RCSR
						v.ovf := avs_s1_writedata(2);
						v.frame := avs_s1_writedata(3);
						v.parerr := avs_s1_writedata(4);
					  v.rxen := avs_s1_writedata(7);
					when "001" =>	 -- Receiver Buffer	RBUF
						
					when "010" =>   -- Transmitter Status XCSR 
					  v.break := avs_s1_writedata(2);
						v.txen := avs_s1_writedata(7);
					when "011" =>	 -- Transmitter Buffer XBUF 
					  v.thold := avs_s1_writedata(7 downto 0);
						v.thempty := '0';
					when "100" =>  -- Control CTRL
			      v.pen := avs_s1_writedata(0);
			      v.pev := avs_s1_writedata(1);	
					when "101" =>  -- Baud Rate
					  v.brate := avs_s1_writedata(r.brate'range);	
						v.prescaler := (others => '0');
					when "110" =>  -- Interrupt Enable
		        v.ie.error := avs_s1_writedata(0);
		        v.ie.rxfull := avs_s1_writedata(1);
		        v.ie.txempty := avs_s1_writedata(2);
					when "111" =>  -- Interrupt Flags Clrear
		        v.iff.error := avs_s1_writedata(0);
		        v.iff.rxfull := avs_s1_writedata(1);
		        v.iff.txempty := avs_s1_writedata(2);					
					when others => null;
				end case;
			end if;
			if avs_s1_read = '1' then
				case avs_s1_address is
					when "000" =>	 -- Receiver Status RCSR
					  avs_s1_readdata(0) <= r.dready;
					  avs_s1_readdata(1) <= r.ovf or r.frame or r.parerr;
					  avs_s1_readdata(2) <= r.ovf;
					  avs_s1_readdata(3) <= r.frame;
					  avs_s1_readdata(4) <= r.parerr;
						avs_s1_readdata(7) <= r.rxen;
					when "001" =>	 -- Receiver Buffer	RBUF
					  avs_s1_readdata(7 downto 0) <= r.rhold;	
						v.read_buf := '1';
					when "010" =>   -- Transmitter Status XCSR
						avs_s1_readdata(0) <= r.thempty;
					  avs_s1_readdata(2) <= r.break; 	
						avs_s1_readdata(7) <= r.txen;
					when "011" =>	 -- Transmitter Buffer XBUF

					when "100" =>  -- Control CTRL
						avs_s1_readdata(0) <= r.pen;
						avs_s1_readdata(1) <= r.pev;
					when "101" =>  -- Baud Rate
					  avs_s1_readdata(r.brate'range) <= r.brate;
						
					when "110" =>  -- Interrupt Enable
					  avs_s1_readdata(0) <= r.ie.error;
					  avs_s1_readdata(1) <= r.ie.rxfull;
						avs_s1_readdata(2) <= r.ie.txempty;
					when "111" =>  -- Interrupt Flags
					  avs_s1_readdata(0) <= r.iff.error;
					  avs_s1_readdata(1) <= r.iff.rxfull;
						avs_s1_readdata(2) <= r.iff.txempty;					
					when others => null;
				end case;				
			end if;	
		else
			if r.read_buf = '1' then
				v.read_buf := '1';
	      v.dready := '0';				
			end if;	
		end if;	 
		
			
		next_r <= v;
	end process next_state_proc;	

	process(reset, clk)
	begin
		if reset = '1' then
			r.rx_state <= st_idle;
			r.tx_state <= st_idle;

			r.rxen <= '1';
			r.txen <= '1'; 	
			
			r.dready <= '0';
      r.rsempty <= '1';
      r.tsempty <= '1';
      r.thempty <= '1';
			r.ovf <= '0';	
      r.break <= '0';
      r.parerr <= '0';
      r.frame <= '0';			
			
      r.rhold <= (others => '0');
      r.rshift <= (others => '0');
      r.tshift(r.tshift'high downto 1) <= (others => '0');
			r.tshift(0) <= '1';
      r.thold <= (others => '0');
      r.rxf <= (others => '0');			

			r.prescaler <= (others => '0');
      r.rxclk <= (others => '0');
		  r.rxtick <= '0';
      r.txclk <= (others => '0');
      r.txtick <= '0';
			r.rxdb <= (others => '0');
			r.dpar <= '0';
			
			r.tpar <= '0';
			
			r.tick <= '0';
			
			r.brate <= CONV_STD_LOGIC_VECTOR(5207, r.brate'length); -- 9600 bit/sec on 50 MHz clk	
			r.pen <= '0';
			r.pev <= '0';
			
		  r.ie.error <= '0';
		  r.ie.rxfull <= '0';
		  r.ie.txempty <= '0';

		  r.iff.error <= '0';
		  r.iff.rxfull <= '0';
		  r.iff.txempty <= '0';	
			
			r.read_buf <= '0';
		elsif rising_edge(clk) then	
			r <= next_r;
		end if;
	end process;

  txd <= r.tshift(0); -- or r.loopb;
  intr <= r.iff.error or r.iff.rxfull or r.iff.txempty;	
end behaviour;
