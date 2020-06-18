-- Project Sorcerer - RISC-V core
--
-- Branch Predictor
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
use work.cache_utils.all;
use work.pipeline_iface.all;
use work.csr.all;
use work.iface.all;

entity branch_predictor is
	generic
	(
	  ENABLE_C: natural range 0 to 1 := 0;     -- Compressed extension
		ENABLE_BPRED_STAT: natural range 0 to 1 := 0  -- Branch prediction statistics
	);
	port
	(
		clk : in std_logic;
		reset : in std_logic;
		
		din: in bpred_in_type;
		dout: out bpred_out_type;
		upd_in: in bpred_update_type;
		
		csr_in: in extern_csr_in_type;
		readdata: out std_logic_vector(WORD_SIZE-1 downto 0)		
	);	
end branch_predictor;

architecture behaviour of branch_predictor is
	signal bht_in: bht_in_type;
	signal bht_out: bht_data_type;
	signal bht_upd_in: bht_update_type;

	signal ras_in: ras_in_type;
	signal ras_out: ras_out_type;

  signal btb_in: btb_in_type;
	signal btb_out: btb_out_type;
	signal btb_upd_in: btb_update_type;
	
	subtype counter_type is std_logic_vector(47 downto 0);
	type bpred_regs_type is record
    addr_hits: counter_type; -- number of correct addr-predictions
		addr_misses: counter_type; -- number of incorrect addr-predictions
    dir_hits: counter_type;  -- number of correct dir-predictions	
		dir_misses: counter_type;    -- number of incorrect predictions
    
	end record;	
	
	signal r, next_r: bpred_regs_type;
begin
	bht1: bht
	generic map
	(
		ENABLE_C => ENABLE_C
	)
	port map
	(
		clk => clk,
		reset => reset,
		din => bht_in,
		dout => bht_out,
		upd_in => bht_upd_in
	);
	
  ras1: ras
	generic	map
	(
	  DEPTH => RAS_DEPTH,
		ENABLE_C => ENABLE_C
  )
	port map
	(
		clk => clk,
		reset => reset,
		din => ras_in,
		dout => ras_out
	);	

  btb1: btb
	generic	map
	(
		ENABLE_C => ENABLE_C
	)
	port map
	(
		clk => clk,
		reset => reset,
		din => btb_in,
		dout => btb_out,
		upd_in => btb_upd_in
	);	 

	bht_in.next_pc <= din.next_pc;
	bht_in.update <= '0';
	dout.bht_data <= bht_out;
	
	ras_in.push <= upd_in.insn_call;
	ras_in.pop <= '1' when upd_in.insn_return = '1' and upd_in.btb_set.hit_vec /= 0 else '0';
	ras_in.flush <= '0';
	ras_in.pc <= upd_in.ret_pc;
	
	btb_in.next_pc <= din.next_pc;
	btb_in.pc <= din.pc;
	btb_upd_in.update <= upd_in.insn_cond_branch or upd_in.insn_jump or upd_in.insn_call;
	btb_upd_in.pc <= upd_in.pc;
	btb_upd_in.target_pc <= upd_in.target_pc;
	btb_upd_in.btb_set <= upd_in.btb_set;
	btb_upd_in.branch_type <= BT_RETURN when upd_in.insn_return = '1'
	  else BT_CONDITIONAL when upd_in.insn_cond_branch = '1'
		else BT_UNCONDITIONAL when upd_in.insn_jump = '1' or upd_in.insn_call = '1'
		else BT_EMPTY;	
			
	with btb_out.branch_type select dout.target_pc <= 
	  ras_out.top_pc when BT_RETURN,
		btb_out.target_pc when BT_CONDITIONAL | BT_UNCONDITIONAL,
		btb_out.target_pc when others;
	dout.hit <= '1' when din.fetch = '1' and btb_out.btb_set.hit_vec /= 0 
	    and btb_out.branch_type /= BT_EMPTY 
		else '0';
		
	next_state_proc: process(r, upd_in)
	  variable v: bpred_regs_type; 
		variable rd_data: std_logic_vector(WORD_SIZE-1 downto 0);
	begin
		v := r;
		rd_data := (others => '0');
		if ENABLE_BPRED_STAT = 1 then
			if upd_in.insn_cond_branch = '1' then
				if is_taken(upd_in.bht_data.state) = upd_in.branch_taken then
					v.dir_hits := r.dir_hits + 1;
				else	
					v.dir_misses := r.dir_misses + 1;
				end if;	
			end if;	
			if (upd_in.insn_jump or upd_in.insn_call or upd_in.insn_return) = '1' then
				if upd_in.mispredict = '1' then
					v.addr_misses := r.addr_misses + 1;
				else	
				  v.addr_hits := r.addr_hits + 1;
				end if;	
			end if;	
			if upd_in.insn_jump = '1' or upd_in.insn_call = '1' then
				-- TODO
			end if;
			if upd_in.insn_return = '1' then
				-- TODO
			end if;	
			
		  case csr_in.address is
        when CSR_bpred_counter1	=>
				  rd_data := r.addr_hits(31 downto 0);
					if csr_in.write = '1' then
						v.addr_hits(31 downto 0) := csr_in.writedata;
					end if;	
				when CSR_bpred_counter1h	=>
				  rd_data(counter_type'high-32 downto 0) := r.addr_hits(counter_type'high downto 32);
					if csr_in.write = '1' then
						v.addr_hits(counter_type'high downto 32) := csr_in.writedata(counter_type'high-32 downto 0);
					end if;
        when CSR_bpred_counter2	=>
				  rd_data := r.addr_misses(31 downto 0);
					if csr_in.write = '1' then
						v.addr_misses(31 downto 0) := csr_in.writedata;
					end if;	
				when CSR_bpred_counter2h	=>
				  rd_data(counter_type'high-32 downto 0) := r.addr_misses(counter_type'high downto 32);
					if csr_in.write = '1' then
						v.addr_misses(counter_type'high downto 32) := csr_in.writedata(counter_type'high-32 downto 0);
					end if;			
       when CSR_bpred_counter3	=>
				  rd_data := r.dir_hits(31 downto 0);
					if csr_in.write = '1' then
						v.dir_hits(31 downto 0) := csr_in.writedata;
					end if;	
				when CSR_bpred_counter3h	=>
				  rd_data(counter_type'high-32 downto 0) := r.dir_hits(counter_type'high downto 32);
					if csr_in.write = '1' then
						v.dir_hits(counter_type'high downto 32) := csr_in.writedata(counter_type'high-32 downto 0);
					end if;
        when CSR_bpred_counter4	=>
				  rd_data := r.dir_misses(31 downto 0);
					if csr_in.write = '1' then
						v.dir_misses(31 downto 0) := csr_in.writedata;
					end if;	
				when CSR_bpred_counter4h	=>
				  rd_data(counter_type'high-32 downto 0) := r.dir_misses(counter_type'high downto 32);
					if csr_in.write = '1' then
						v.dir_misses(counter_type'high downto 32) := csr_in.writedata(counter_type'high-32 downto 0);
					end if;					
				when others => null;
			end case;
		end if;	
		readdata <= rd_data;
		next_r <= v;
	end process next_state_proc;	
	
	process(reset, clk)
	begin
		if reset = '1' then
	    r.addr_hits <= (others => '0');
			r.addr_misses <= (others => '0');
	    r.dir_hits <= (others => '0');
			r.dir_misses <= (others => '0');
	    			
		else	
			r <= next_r;
		end if;	
	end process;	
end behaviour;
