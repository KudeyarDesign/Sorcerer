-- Project Sorcerer - RISC-V core
--
-- Hardware Breakpoints
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
use work.config.all;
use work.csr.all;
use work.pipeline_iface.all;

entity hw_breakpoints is
	generic
	(
	  INSN_BP_NUM: natural range 1 to 16 := 4;
		DATA_BP_NUM: natural range 1 to 16 := 2;
		ENABLE_FULL_READ: natural range 0 to 1 := 1
	);
  port
  (
	  clk : in std_logic;
		reset : in std_logic;
		
		din: in hw_breakpoints_in_type;
		dout: out hw_breakpoints_out_type;
		
		csr_in: in extern_csr_in_type;
		readdata: out std_logic_vector(WORD_SIZE-1 downto 0)
	);	
end hw_breakpoints;

architecture behaviour of hw_breakpoints is
  constant BP_NUM: natural:= INSN_BP_NUM + DATA_BP_NUM; 

  type bpcontrol_type is record
    owner: std_logic_vector(1 downto 0);
		loadsup: std_logic;
		storesup: std_logic; 
		execsup: std_logic; 
		asup: std_logic; 
		arangesup: std_logic; 
		amasksup: std_logic;
    dsup: std_logic; 
		drangesup: std_logic; 
		dmasksup: std_logic;		
		matched: std_logic; 
		action: std_logic_vector(2 downto 0);
		loaden: std_logic; 
		storeen: std_logic;
    execen: std_logic; 
		aen: std_logic; 
		arangeen: std_logic; 
		amasken: std_logic;
		den: std_logic;
		drangeen: std_logic;
		dmasken: std_logic;
	end record;	
	
	type bp_type is record
	  bpcontrol: bpcontrol_type;					       -- Breakpoint Control
		loaddress: std_logic_vector(31 downto 0);  -- Breakpoint Low Address
		hiaddress: std_logic_vector(31 downto 0);	 -- Breakpoint High Address
		lodata: std_logic_vector(31 downto 0);		 -- Breakpoint Low Data
		hidata: std_logic_vector(31 downto 0);		 -- Breakpoint High Data
	end record;
	
	type bp_array is array(0 to BP_NUM-1) of bp_type;

  type bp_regs_type is record
		bp: std_logic_vector(11 downto 0);  -- Breakpoint Select
		bpoints: bp_array;                  -- Breakpoints 
	end record;
	
  signal r, next_r: bp_regs_type;	
begin
	next_state_proc: process(r, din, csr_in)
	  variable pc_match: std_logic;
		variable mem_match: std_logic;
		variable rd_data: std_logic_vector(31 downto 0);
		variable bp_valid: std_logic;
		variable bp_idx: integer;
		variable v: bp_regs_type;	
		
		procedure bp_match(bp: inout bp_type) is
	    variable bp_pc_match: std_logic;
		  variable bp_amatch: std_logic;
			variable bp_dmatch: std_logic;
			variable bp_omatch: std_logic;
			variable bp_mem_match: std_logic;
		begin														
		  bp_pc_match := '0';
		  bp_amatch := '0';
			bp_dmatch := '0';
			bp_omatch := '0';
			if bp.bpcontrol.execsup = '1' and din.exec = '1' then
				if bp.bpcontrol.asup = '1' then
					if bp.bpcontrol.aen = '1' then
						if din.pc = bp.loaddress then
							bp_pc_match := '1';
						end if;	
					end if;
				end if;	
				if bp.bpcontrol.arangesup = '1' then
					if bp.bpcontrol.arangeen = '1' then
					  if din.pc >= bp.loaddress and din.pc < bp.hiaddress then
							bp_pc_match := '1';
						end if;	
					end if;	
				end if;	
				if bp.bpcontrol.amasksup = '1' then
					if bp.bpcontrol.amasken = '1' then
						if din.pc & bp.hiaddress = bp.loaddress then
							bp_pc_match := '1';
						end if;
					end if;	
				end if;	
			end if;	
			if bp.bpcontrol.loadsup = '1' or bp.bpcontrol.storesup = '1' then
				if bp.bpcontrol.aen = '0' and bp.bpcontrol.arangeen = '0' and 
					   bp.bpcontrol.amasken = '0' then
					bp_amatch := '1';
				end if;	
				if bp.bpcontrol.asup = '1' then
					if bp.bpcontrol.aen = '1' then
						if din.pc = bp.loaddress then
							bp_amatch := '1';
						end if;	
					end if;
				end if;	
				if bp.bpcontrol.arangesup = '1' then
					if bp.bpcontrol.arangeen = '1' then
					  if din.pc >= bp.loaddress and din.pc < bp.hiaddress then
							bp_amatch := '1';
						end if;	
					end if;	
				end if;	
				if bp.bpcontrol.amasksup = '1' then
					if bp.bpcontrol.amasken = '1' then
						if din.pc & bp.hiaddress = bp.loaddress then
							bp_amatch := '1';
						end if;
					end if;	
				end if;
				
				if bp.bpcontrol.den = '0' and bp.bpcontrol.drangeen = '0' and 
					   bp.bpcontrol.dmasken = '0' then
					bp_dmatch := '1';
				end if;
				if bp.bpcontrol.dsup = '1' then
					if bp.bpcontrol.den = '1' then
						if din.pc = bp.lodata then
							bp_dmatch := '1';
						end if;	
					end if;
				end if;	
				if bp.bpcontrol.drangesup = '1' then
					if bp.bpcontrol.drangeen = '1' then
					  if din.pc >= bp.lodata and din.pc < bp.hidata then
							bp_dmatch := '1';
						end if;	
					end if;	
				end if;	
				if bp.bpcontrol.amasksup = '1' then
					if bp.bpcontrol.amasken = '1' then
						if din.pc & bp.hidata = bp.lodata then
							bp_dmatch := '1';
						end if;
					end if;	
				end if;				
			end if;	 
			
			if bp.bpcontrol.loadsup = '1' then
				if bp.bpcontrol.loaden = '1' and din.load = '1' then
					bp_omatch := '1';					
				end if;	
			end if;
			if bp.bpcontrol.storesup = '1' then
				if bp.bpcontrol.storeen = '1' and din.store = '1' then
					bp_omatch := '1';					
				end if;	
			end if;			
			
			bp_mem_match := bp_amatch and bp_dmatch and bp_omatch;
			bp.bpcontrol.matched := bp.bpcontrol.matched or bp_pc_match or bp_mem_match;
			pc_match := pc_match or bp_pc_match;
			mem_match := mem_match or bp_mem_match;
		end procedure bp_match;	
	begin
		v := r;
		pc_match := '0';
		mem_match := '0';
		for i in 0 to BP_NUM-1 loop	
			bp_match(v.bpoints(i));
		end loop;					 
		
		if r.bp < BP_NUM then
			bp_valid := '1';
		else	
			bp_valid := '0';
		end if;	
		bp_idx := CONV_INTEGER(r.bp);
		rd_data := (others => '0');
		case csr_in.address is
			when CSR_bpselect =>   -- Breakpoint Select
			  rd_data(r.bp'range) := r.bp;
				if csr_in.write = '1' then
					v.bp := csr_in.writedata(r.bp'range);
				end if;	
			when CSR_bpcontrol =>	 -- Breakpoint Control
			  if bp_valid = '1' then
					rd_data(31 downto 30) := r.bpoints(bp_idx).bpcontrol.owner;
					rd_data(26) := r.bpoints(bp_idx).bpcontrol.loadsup;
					rd_data(25) := r.bpoints(bp_idx).bpcontrol.storesup;
					rd_data(24) := r.bpoints(bp_idx).bpcontrol.execsup;
					rd_data(23) := r.bpoints(bp_idx).bpcontrol.asup;
					rd_data(22) := r.bpoints(bp_idx).bpcontrol.arangesup;
					rd_data(21) := r.bpoints(bp_idx).bpcontrol.amasksup;
					rd_data(19) := r.bpoints(bp_idx).bpcontrol.dsup;
					rd_data(18) := r.bpoints(bp_idx).bpcontrol.drangesup;
					rd_data(17) := r.bpoints(bp_idx).bpcontrol.dmasksup;
					rd_data(15) := r.bpoints(bp_idx).bpcontrol.matched;
					rd_data(14 downto 12) := r.bpoints(bp_idx).bpcontrol.action;
					rd_data(10) := r.bpoints(bp_idx).bpcontrol.loaden;
					rd_data(9) := r.bpoints(bp_idx).bpcontrol.storeen;
					rd_data(8) := r.bpoints(bp_idx).bpcontrol.execen;
					rd_data(7) := r.bpoints(bp_idx).bpcontrol.aen;
					rd_data(6) := r.bpoints(bp_idx).bpcontrol.arangeen;
					rd_data(5) := r.bpoints(bp_idx).bpcontrol.amasken;
					rd_data(3) := r.bpoints(bp_idx).bpcontrol.den;
					rd_data(2) := r.bpoints(bp_idx).bpcontrol.drangeen;
					rd_data(1) := r.bpoints(bp_idx).bpcontrol.dmasken;
				  if csr_in.write = '1' then
					  v.bpoints(bp_idx).bpcontrol.owner := csr_in.writedata(31 downto 30);
						v.bpoints(bp_idx).bpcontrol.matched := csr_in.writedata(15);
						v.bpoints(bp_idx).bpcontrol.action := csr_in.writedata(14 downto 12);
						v.bpoints(bp_idx).bpcontrol.loaden := csr_in.writedata(10);
						v.bpoints(bp_idx).bpcontrol.storeen := csr_in.writedata(9);
						v.bpoints(bp_idx).bpcontrol.execen := csr_in.writedata(8);
						v.bpoints(bp_idx).bpcontrol.aen := csr_in.writedata(7);
						v.bpoints(bp_idx).bpcontrol.arangeen := csr_in.writedata(6);
						v.bpoints(bp_idx).bpcontrol.amasken := csr_in.writedata(5);
						v.bpoints(bp_idx).bpcontrol.den := csr_in.writedata(3);
						v.bpoints(bp_idx).bpcontrol.drangeen := csr_in.writedata(2);
						v.bpoints(bp_idx).bpcontrol.dmasken := csr_in.writedata(1);
  				end if;					
				end if;
			when CSR_bploaddr =>   -- Breakpoint Low Address
			  if bp_valid = '1' then 
				  if ENABLE_FULL_READ = 1 then
					  rd_data := r.bpoints(bp_idx).loaddress;
					end if;	
				  if csr_in.write = '1' then
					 	v.bpoints(bp_idx).loaddress := csr_in.writedata;
  				end if;					
				end if;	
			when CSR_bphiaddr =>     -- Breakpoint High Address 
			  if bp_valid = '1' then
				  if ENABLE_FULL_READ = 1 then
					  rd_data := r.bpoints(bp_idx).hiaddress;
					end if;	
				  if csr_in.write = '1' then
					 	v.bpoints(bp_idx).hiaddress := csr_in.writedata;
  				end if;				
				end if;	
			when CSR_bplodata =>   -- Breakpoint Low Data
			  if bp_valid = '1' then
				  if ENABLE_FULL_READ = 1 then
					  rd_data := r.bpoints(bp_idx).lodata;
					end if;	
				  if csr_in.write = '1' then
					 	v.bpoints(bp_idx).lodata := csr_in.writedata;
  				end if;					
				end if;	
			when CSR_bphidata => 		 -- Breakpoint High Data
			  if bp_valid = '1' then
				  if ENABLE_FULL_READ = 1 then
					  rd_data := r.bpoints(bp_idx).hidata;
					end if;	
				  if csr_in.write = '1' then
					 	v.bpoints(bp_idx).hidata := csr_in.writedata;
  				end if;					
				end if;					
			when others => null;
		end case;	
			
		readdata <= rd_data;	
		dout.pc_match <= pc_match;
		dout.mem_match <= mem_match;
		dout.match <= pc_match or mem_match;
		dout.raise_exception_exec <= '0';
		dout.enter_debug_mode_exec <= '0';
		dout.raise_exception_mem <= '0';
		dout.enter_debug_mode_mem <= '0';		
		dout.start_tracing <= '0';
		dout.stop_tracing <= '0';
		dout.emit_trace_data <= '0'; 
		if bp_valid = '1' and pc_match = '1' then
		  case r.bpoints(bp_idx).bpcontrol.action is
				when "001" =>  dout.raise_exception_exec <= '1';
				when "010" =>  dout.enter_debug_mode_exec <= '1';
				when "011" =>  dout.start_tracing <= '1';
				when "100" =>  dout.stop_tracing <= '1';
				when "101" =>  dout.emit_trace_data <= '1';
			  when others => null;
		  end case;	
		end if;	
		if bp_valid = '1' and mem_match = '1' then
		  case r.bpoints(bp_idx).bpcontrol.action is
				when "001" =>  dout.raise_exception_mem <= '1';
				when "010" =>  dout.enter_debug_mode_mem <= '1';
--				when "011" =>  dout.start_tracing <= '1';
--				when "100" =>  dout.stop_tracing <= '1';
--				when "101" =>  dout.emit_trace_data <= '1';
			  when others => null;
		  end case;	
		end if;		
		next_r <= v;
	end process next_state_proc;	
	
	process(reset, clk)
	begin
		if reset = '1' then
			for i in 0 to BP_NUM-1 loop
	      r.bpoints(i).bpcontrol.owner <= "00";
			  r.bpoints(i).bpcontrol.matched <= '0'; 
			  r.bpoints(i).bpcontrol.action <= "000";
			  r.bpoints(i).bpcontrol.loaden <= '0'; 
			  r.bpoints(i).bpcontrol.storeen <= '0';
	      r.bpoints(i).bpcontrol.execen <= '0'; 
			  r.bpoints(i).bpcontrol.aen <= '0'; 
			  r.bpoints(i).bpcontrol.arangeen <= '0'; 
			  r.bpoints(i).bpcontrol.amasken <= '0';
			  r.bpoints(i).bpcontrol.den <= '0';
			  r.bpoints(i).bpcontrol.drangeen <= '0';
			  r.bpoints(i).bpcontrol.dmasken <= '0';
			end loop;
			-- PC address breakpoints
			for i in 0 to INSN_BP_NUM-1 loop
				r.bpoints(i).bpcontrol.loadsup <= '0';
				r.bpoints(i).bpcontrol.storesup <= '0';
				r.bpoints(i).bpcontrol.execsup <= '1';
				r.bpoints(i).bpcontrol.asup <= '1';
				r.bpoints(i).bpcontrol.arangesup <= '0';
				r.bpoints(i).bpcontrol.amasksup <= '1';
				r.bpoints(i).bpcontrol.dsup <= '0';
				r.bpoints(i).bpcontrol.drangesup <= '0';
				r.bpoints(i).bpcontrol.dmasksup <= '0';
			end loop;		
			-- Memory access breakppoints
			for i in INSN_BP_NUM to BP_NUM-1 loop
				r.bpoints(i).bpcontrol.loadsup <= '1';
				r.bpoints(i).bpcontrol.storesup <= '1';
				r.bpoints(i).bpcontrol.execsup <= '0';
				r.bpoints(i).bpcontrol.asup <= '1';
				r.bpoints(i).bpcontrol.arangesup <= '0';
				r.bpoints(i).bpcontrol.amasksup <= '0';
				r.bpoints(i).bpcontrol.dsup <= '1';
				r.bpoints(i).bpcontrol.drangesup <= '0';
				r.bpoints(i).bpcontrol.dmasksup <= '1';				
			end loop;	
		elsif rising_edge(clk) then	
			r <= next_r;
		end if;
	end process;
end behaviour;
