-- Project Sorcerer - RISC-V core
--
-- Trace Module
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
use work.utils.all;
use work.pipeline_iface.all;
use work.iface.all;
use work.csr.all;

entity trace is
	generic
	(
	  FIFO_SIZE: natural range 128 to 65536 := 2048; -- Number of bytes
		FIFO_WIDTH: natural range 32 to 512 := 128;		 -- Number of bits
		INIT_BUF_START: integer := 0;
		INIT_BUF_END: integer	:= 0
	);
  port
  (
	  clk: in std_logic;
		reset: in std_logic;
		
		din: in trace_in_type;
		dout: out trace_out_type;
		cmd_in: in trace_cmd_in_type;
		csr_in: in trace_csr_in_type;
    csr_out: out trace_csr_out_type;
		bus_in: in trace_bus_in_type;
    bus_out: out trace_bus_out_type
  );		
end trace;

architecture behaviour of trace is
  constant MAX_PACKET_WIDTH: natural := 2 + WORD_SIZE/4;  -- Number of 4-bit slices in packet
  constant TRACE_WIDTH: natural := 2* 4* MAX_PACKET_WIDTH + 2*4;  -- Maximum number of bits in trace
	constant TRACE_SLICE_NUM: natural := TRACE_WIDTH/4;	 -- Maximum number of slices in trace
	constant TRACE_BITS: natural := ceil_log2(TRACE_SLICE_NUM);	 -- Number of bits in slice counters
	constant FLUSH_LEN: natural := TRACE_BUS_WIDTH*TRACE_BURST_LEN/TRACE_WIDTH + 1;  -- Number of flush traces after stop
	constant FLUSH_WAIT_CYCLES: natural := 64;
	
	constant FIFO_DEPTH: natural := 8*FIFO_SIZE/FIFO_WIDTH;

  subtype event_type is std_logic_vector(3 downto 0);
	
	constant EVENT_Nop: event_type := "0000";        -- Packet that indicates no data
	constant EVENT_PC:  event_type := "0001";        -- Program Counter
	constant EVENT_BranchTaken: event_type := "0010";
	constant EVENT_BranchNotTaken: event_type := "0011";
	constant EVENT_TraceEnabled: event_type := "0100";
	constant EVENT_TraceDisabled: event_type := "0101";
	constant EVENT_PriviledgeLevel: event_type := "0110";
	constant EVENT_Reserved1: event_type := "0111";  -- Reserved for future standards
	constant EVENT_LoadAddress: event_type := "1000";
	constant EVENT_StoreAddress: event_type := "1001";	
	constant EVENT_LoadData: event_type := "1010";
	constant EVENT_StoreData: event_type := "1011";
	constant EVENT_Timestamp: event_type := "1100";	
	constant EVENT_Reserved2: event_type := "1101";	 -- Reserved for future standards
	constant EVENT_Custom1: event_type := "1110";	   -- Reserved for custom trace data
	constant EVENT_Custom2: event_type := "1111";	   -- Reserved for custom trace data	
		
	type trace_type is record
		wrapped: std_logic;
		emittimestamp: std_logic;
		emitstoredata: std_logic;
		emitloaddata: std_logic;
		emitstoreaddr: std_logic;
		emitloadaddr: std_logic;
		emitpriv: std_logic;
		emitbranch: std_logic;
		emitpc: std_logic;
		fullaction: std_logic_vector(1 downto 0);
		destination: std_logic_vector(1 downto 0);
		stall: std_logic;
		discard: std_logic;
	end record;	
	
	type trace_csr_regs_type is record
		trace: trace_type;
		tbufstart: std_logic_vector(WORD_SIZE-1 downto 0);
		tbufend: std_logic_vector(WORD_SIZE-1 downto 0);
		tbufwrite: std_logic_vector(WORD_SIZE-1 downto 0);
	end record;

	type state_type is (st_idle, st_trace, st_flush, st_wait_store);
	type bus_state_type is (st_bus_idle, st_bus_request_write, st_bus_write);
	type trace_regs_type is record
		state: state_type;
		csr: trace_csr_regs_type;
		trace_in1: trace_in_type;
		trace_in2: trace_in_type;
		pc_len: std_logic_vector(3 downto 0);
		pc_data_len: std_logic_vector(3 downto 0);
		branch_target_len: std_logic_vector(3 downto 0);
		branch_target_data_len: std_logic_vector(3 downto 0);
	address_len: std_logic_vector(3 downto 0);
		readdata_len: std_logic_vector(3 downto 0);
		writedata_len: std_logic_vector(3 downto 0);
		timestamp_len: std_logic_vector(3 downto 0);		
		last_pc: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		last_address: std_logic_vector(VADDRESS_WIDTH-1 downto 0);
		last_timestamp: std_logic_vector(63 downto 0);
		bus_state: bus_state_type;
		flush_counter: std_logic_vector(ceil_log2(FLUSH_WAIT_CYCLES)-1 downto 0);
		counter: std_logic_vector(log2(TRACE_BURST_LEN) downto 0);
	end record;	
	
	signal r, next_r: trace_regs_type;
	
	signal fifo_trace : std_logic_vector(TRACE_WIDTH-1 downto 0);
	signal fifo_trace_len : std_logic_vector(ceil_log2(TRACE_WIDTH)-1 downto 0);
	signal fifo_flush: std_logic;
	signal fifo_full: std_logic;
  signal fifo_num_word : std_logic_vector(ceil_log2(FIFO_DEPTH*FIFO_WIDTH/TRACE_BUS_WIDTH)-1 downto 0);
	signal fifo_rdreq: std_logic;
	signal fifo_wrreq: std_logic;
	
	function get_value_sequence_len(value: in std_logic_vector; 
	    last_value: in std_logic_vector) return std_logic_vector is
		variable len: std_logic_vector(3 downto 0);
	begin
		len := "0000";
		for i in 0 to value'length/4-1 loop
			if value((i+1)*4-1 downto i*4) /= last_value((i+1)*4-1 downto i*4) then
				len := CONV_STD_LOGIC_VECTOR(i, 4);
			end if;	
		end loop;
		return len;
	end function get_value_sequence_len;
	
	function get_value_sequence_len(value: in std_logic_vector) return std_logic_vector is
	  variable len: std_logic_vector(3 downto 0);
		variable sign: std_logic_vector(3 downto 0);
	begin
		len := "0000";
		if value(value'high) = '1' then
			sign := "1111";
		else	
			sign := "0000";
		end if;	
		for i in 1 to value'length/4-1 loop
			if value((i+1)*4-1 downto i*4) /= sign then	
				if value(i*4-1) = value(value'high) then
				  len := CONV_STD_LOGIC_VECTOR(i, 4);
				else
					len := CONV_STD_LOGIC_VECTOR(i+1, 4);
				end if;	
			end if;	
		end loop;
		return len;
	end function get_value_sequence_len;	
	
begin
  fifo1: trace_fifo
	generic	map
	(
	  TRACE_WIDTH => TRACE_WIDTH,
		FIFO_WIDTH => FIFO_WIDTH,
		FIFO_DEPTH => FIFO_DEPTH,
		OUT_WIDTH => TRACE_BUS_WIDTH,	
		DEVICE_FAMILY => DEVICE_FAMILY
	)
	port map
	(
	  clk => clk,
	  reset => reset,
		trace => fifo_trace,
		trace_len => fifo_trace_len,
		trace_wrreq	=> fifo_wrreq,
		flush => fifo_flush,
		bus_rdreq	=> fifo_rdreq,
		full => fifo_full,
		q	=> bus_out.writedata,
		num_word => fifo_num_word
	);	
	
	next_state_proc: process(r, din, cmd_in, csr_in, bus_in, fifo_full, fifo_num_word)
	  variable v: trace_regs_type;
		variable rd_data: std_logic_vector(WORD_SIZE-1 downto 0);
		variable trace_len: std_logic_vector(TRACE_BITS-1 downto 0);
		variable trace: std_logic_vector(TRACE_WIDTH-1 downto 0);
		variable address_ovf: std_logic;
		variable stall1: std_logic;
		variable stall2: std_logic;
		
		procedure emit(event: in event_type) is
		begin																								
			for i in 0 to 3 loop
				trace(i) := event(i);
			end loop;	
			trace_len := CONV_STD_LOGIC_VECTOR(1, TRACE_BITS); 
			fifo_wrreq <= '1';
		end procedure emit;

		function get_packet(event: in event_type; data: in std_logic_vector; len: in std_logic_vector(3 downto 0)) 
		    return std_logic_vector is
			variable packet: std_logic_vector(MAX_PACKET_WIDTH*4-1 downto 0);
		begin	
			packet := (others => '0');
			for i in 0 to 3 loop
				packet(i) := event(i);
				packet(i + 4) := len(i);
			end loop;
			for n in 0 to MAX_PACKET_WIDTH-3 loop
				if n <= len then
  			  for i in 0 to 3 loop 
--	  			  if 4* n + i < data'length then 
		  		    packet(4*n + i + 8) := data(4*n + i);
--			  	  end if;	
  			  end loop;	
				end if;
			end loop;	
			return packet;
		end function get_packet;	

		function enable_emitPC(r: trace_regs_type) return boolean is
		begin
      return -- r.csr.trace.emitbranch = '0' and 
			   r.csr.trace.emitpc = '1';			
		end function enable_emitPC;	
		
		procedure emit(event: in event_type; data: in std_logic_vector; len: in std_logic_vector(3 downto 0)) is
		begin
			trace(MAX_PACKET_WIDTH*4-1 downto 0) := get_packet(event, data, len);
			trace_len := zero_extend(len, TRACE_BITS) + 3;
			fifo_wrreq <= '1';
		end procedure emit;
		
		procedure emit(event1: in event_type; data1: in std_logic_vector; len1: in std_logic_vector(3 downto 0);
		               event2: in event_type; data2: in std_logic_vector; len2: in std_logic_vector(3 downto 0)) is
			variable packet2: std_logic_vector(MAX_PACKET_WIDTH*4-1 downto 0);
			variable shift_len: std_logic_vector(ceil_log2(MAX_PACKET_WIDTH)+1 downto 0);
			variable shift_packet2: std_logic_vector(2*MAX_PACKET_WIDTH*4-1 downto 0);
		begin
			emit(event1, data1, len1);
			packet2 := get_packet(event2, data2, len2);
			shift_len(1 downto 0) := "00";
			shift_len(shift_len'high downto 2) := trace_len(shift_len'high-2 downto 0);
			shift_packet2 := barrel_shift_l(packet2, shift_len)(shift_packet2'range);
			trace(shift_packet2'range) := trace(shift_packet2'range) or shift_packet2;
			trace_len := trace_len + zero_extend(len2, TRACE_BITS) + 3;
		end procedure emit;	
		
		procedure emitJump is
		begin
			emit(EVENT_PC, r.trace_in2.pc, r.pc_len, 
			     EVENT_PC, r.trace_in2.branch_target, r.branch_target_len);
		end procedure emitJump;
		
		procedure emitTrap(intr: std_logic) is
		  variable packet_priv: std_logic_vector(7 downto 0);
		begin
			packet_priv(3 downto 0) := EVENT_PriviledgeLevel;
			packet_priv(4) := intr;
			packet_priv(6 downto 5) := r.trace_in2.priv;
			packet_priv(7) := r.trace_in2.ie;
			emit(EVENT_PC, r.trace_in2.pc, r.pc_len, 
			    EVENT_PC, r.trace_in2.branch_target, r.branch_target_len);
			for n in 6 to 2*MAX_PACKET_WIDTH loop
				if trace_len = n then
					trace(4*n+7 downto 4*n) := packet_priv;
				end if;	
			end loop;	
			trace_len := trace_len + 2;
		end procedure emitTrap;			
	begin
		v := r;
		if fifo_full = '1' and r.trace_in2.event /= trace_evt_none then
			stall2 := '1';
		else
			stall2 := '0';
		end if;	

		if stall2 = '1' and r.trace_in1.event /= trace_evt_none then
			stall1 := '1';
		else
			stall1 := '0';			
		end if;	
		
		if stall1 = '0' then
			v.trace_in1 := din;
		end if;	
		
		fifo_flush <= '0'; 
		fifo_wrreq <= '0';
		case r.state is
			when st_idle =>
			  v.trace_in1.event := trace_evt_none;
			  if cmd_in.start = '1' then
					v.state := st_trace;
				end if;
			when st_trace =>
			  if cmd_in.stop = '1' then
				  v.flush_counter := (others => '0');
					v.state := st_flush;				
				end if;
			when st_flush =>			
			  v.trace_in1.event := trace_evt_flush;
				if r.flush_counter = FLUSH_LEN-1 then
					v.flush_counter := (others => '0');
					v.state := st_wait_store;
				else	
					v.flush_counter := r.flush_counter + 1;
				end if;	
				if cmd_in.start = '1' then
					v.state := st_trace;
				end if;	
			when st_wait_store =>
			  v.trace_in1.event := trace_evt_none;
				if cmd_in.start = '1' then 
					v.state := st_trace;			
				elsif r.flush_counter = FLUSH_WAIT_CYCLES-1 then
					fifo_flush <= '1';
			    v.state := st_idle;
				end if;	
				v.flush_counter := r.flush_counter + 1;
		end case;		
		
		if stall2 = '0' then
			v.trace_in2 := r.trace_in1;
			v.pc_len := get_value_sequence_len(r.trace_in1.pc, r.last_pc);
			v.branch_target_len := get_value_sequence_len(r.trace_in1.branch_target, r.last_pc);
			v.branch_target_data_len := get_value_sequence_len(r.trace_in1.branch_target);
			v.address_len := get_value_sequence_len(r.trace_in1.address, r.last_address);
			v.readdata_len := get_value_sequence_len(r.trace_in1.readdata); 
			v.writedata_len := get_value_sequence_len(r.trace_in1.writedata);
			v.timestamp_len := get_value_sequence_len(r.trace_in1.timestamp(WORD_SIZE-1 downto 0), 
			  r.last_timestamp(WORD_SIZE-1 downto 0));  -- timestamp truncated to 32 bits
			
			case r.trace_in1.event is
				when trace_evt_none =>  null;
	      when trace_evt_jal =>
				  if enable_emitPC(r) then
					  v.last_pc := r.trace_in1.branch_target;	
					end if;
	      when trace_evt_jalr	=>
				  if enable_emitPC(r) then
						v.last_pc := r.trace_in1.branch_target;
					elsif r.csr.trace.emitstoredata = '1' then
						null;
					end if;			
	      when trace_evt_branch	=>
				  if r.csr.trace.emitbranch = '1' then
					  null;
					elsif r.csr.trace.emitpc = '1' and r.trace_in1.branch_taken = '1' then	
						v.last_pc := r.trace_in1.branch_target;
					end if;
	      when trace_evt_load =>
				  if r.csr.trace.emitloadaddr = '1' then
						v.last_address := r.trace_in1.address;
					end if;	
	      when trace_evt_store =>
				  if r.csr.trace.emitstoreaddr = '1' then
						v.last_address := r.trace_in1.address;
					end if;			
	      when trace_evt_trap =>
				  if enable_emitPC(r) then
						v.last_pc := r.trace_in1.branch_target;
					end if;			
	      when trace_evt_intr =>
				  if enable_emitPC(r) then
						v.last_pc := r.trace_in1.branch_target;
					end if;			
	      when trace_evt_csr_read =>
				  null;
				when trace_evt_csr_write =>
				  null;
	      when trace_evt_drop	=>
				  -- TODO	 
				when trace_evt_flush =>
				  null;
			end case;			
		end if;
		
		trace := (others => '0');	
		trace_len := (others => '0');
		if fifo_full = '0' then
			case r.trace_in2.event is
				when trace_evt_none =>  null;
	      when trace_evt_jal =>
				  if enable_emitPC(r) then
						emitJump;
					end if;
	      when trace_evt_jalr	=>
				  if enable_emitPC(r) then
						emitJump;
					elsif r.csr.trace.emitstoredata = '1' then
						emit(EVENT_StoreData, r.trace_in2.branch_target, r.branch_target_data_len); 
					end if;			
	      when trace_evt_branch	=>
				  if r.csr.trace.emitbranch = '1' then
						if r.trace_in2.branch_taken = '1' then
							emit(EVENT_BranchTaken);
						else	
							emit(EVENT_BranchNotTaken);
						end if;	
					elsif r.csr.trace.emitpc = '1' and r.trace_in2.branch_taken = '1' then	
						emitJump;
					end if;
	      when trace_evt_load =>
				  if r.csr.trace.emitloadaddr = '1' then
						if r.csr.trace.emitloaddata = '1' then	
							emit(EVENT_LoadAddress, r.trace_in2.address, r.address_len,
							     EVENT_LoadData, r.trace_in2.readdata, r.readdata_len);
						else	
							emit(EVENT_LoadAddress, r.trace_in2.address, r.address_len);
						end if;
					elsif r.csr.trace.emitloaddata = '1' then	
						emit(EVENT_LoadData, r.trace_in2.readdata, r.readdata_len); 
					end if;	
	      when trace_evt_store =>
				  if r.csr.trace.emitstoreaddr = '1' then
						if r.csr.trace.emitstoredata = '1' then	
							emit(EVENT_StoreAddress, r.trace_in2.address, r.address_len,
							     EVENT_StoreData, r.trace_in2.writedata, r.writedata_len);
						else	
							emit(EVENT_StoreAddress, r.trace_in2.address, r.address_len);
						end if;
					elsif r.csr.trace.emitstoredata = '1' then	
						emit(EVENT_StoreData, r.trace_in2.writedata, r.writedata_len);
					end if;			
	      when trace_evt_trap =>
				  if enable_emitPC(r) then
						emitTrap('0');
					end if;			
	      when trace_evt_intr =>
				  if enable_emitPC(r) then
						emitTrap('1');
					end if;			
	      when trace_evt_csr_read =>
	        if r.csr.trace.emitloaddata = '1' then	
						emit(EVENT_LoadData, r.trace_in2.readdata, r.readdata_len); 
					end if;			
				when trace_evt_csr_write =>
	        if r.csr.trace.emitstoredata = '1' then	
						emit(EVENT_StoreData, r.trace_in2.writedata, r.writedata_len);
					end if;			
	      when trace_evt_drop	=>
				  -- TODO	
				when trace_evt_flush =>			
				  fifo_wrreq <= '1';
			    trace := (others => '0');
			    trace_len := CONV_STD_LOGIC_VECTOR(TRACE_WIDTH/4, trace_len'length);				  
			end case;	
		end if;
		
		-- CSR read & write
		rd_data := (others => '0');
		case csr_in.address is
      when CSR_trace => 
			  rd_data(24) := r.csr.trace.wrapped;
				rd_data(23) := r.csr.trace.emittimestamp;
				rd_data(22) := r.csr.trace.emitstoredata;
				rd_data(21) := r.csr.trace.emitloaddata;
				rd_data(20) := r.csr.trace.emitstoreaddr;
				rd_data(19) := r.csr.trace.emitloadaddr;
				rd_data(18) := r.csr.trace.emitpriv;
				rd_data(17) := r.csr.trace.emitbranch;
				rd_data(16) := r.csr.trace.emitpc;
				rd_data(9 downto 8) := r.csr.trace.fullaction;
				rd_data(5 downto 4) := r.csr.trace.destination;
				rd_data(2) := r.csr.trace.stall;
				rd_data(0) := '1';	-- supported
				if csr_in.write = '1' then 
				  v.csr.trace.emittimestamp := csr_in.writedata(23);
				  v.csr.trace.emitstoredata := csr_in.writedata(22);
				  v.csr.trace.emitloaddata := csr_in.writedata(21);
				  v.csr.trace.emitstoreaddr := csr_in.writedata(20);
				  v.csr.trace.emitloadaddr := csr_in.writedata(19);
				  v.csr.trace.emitpriv := csr_in.writedata(18);
				  v.csr.trace.emitbranch := csr_in.writedata(17);
				  v.csr.trace.emitpc := csr_in.writedata(16);
				  v.csr.trace.fullaction := csr_in.writedata(9 downto 8);
				  v.csr.trace.destination := csr_in.writedata(5 downto 4); 
				  v.csr.trace.stall := csr_in.writedata(2);
					v.csr.trace.discard := csr_in.writedata(1);
				end if;			  
      when CSR_tbufstart => 
			  rd_data := r.csr.tbufstart;
				if csr_in.write = '1' then
					v.csr.tbufstart := csr_in.writedata;
					v.csr.tbufwrite := csr_in.writedata;
				end if;			
      when CSR_tbufend => 
				rd_data := r.csr.tbufend;
				if csr_in.write = '1' then
					v.csr.tbufend := csr_in.writedata;
				end if;			
      when CSR_tbufwrite => 
			  rd_data := r.csr.tbufwrite;
			when others =>  null;
		end case;
		csr_out.readdata <= rd_data;
		
		-- Bus write
		bus_out.write <= '0'; 
		bus_out.burstcount <= (others => '0');
		bus_out.address <= r.csr.tbufwrite;
		if r.csr.tbufwrite + TRACE_BURST_LEN*TRACE_BUS_WIDTH/8 > r.csr.tbufend then
			address_ovf := '1';
		else	
			address_ovf := '0';
		end if;
		fifo_rdreq <= '0';
		case r.bus_state is
			when st_bus_idle =>
			  if address_ovf = '0' and fifo_num_word >= TRACE_BURST_LEN then
					v.bus_state := st_bus_request_write;
				end if;	
				v.counter := (others => '0');
			when st_bus_request_write =>
			  bus_out.write <= '1'; 
				bus_out.burstcount <= CONV_STD_LOGIC_VECTOR(TRACE_BURST_LEN, bus_out.burstcount'length);
				if bus_in.waitrequest = '0' then
					fifo_rdreq <= '1';
					v.counter := r.counter + 1;
					v.bus_state := st_bus_write;
				end if;				
			when st_bus_write =>
			  bus_out.write <= '1';
				if bus_in.waitrequest = '0' then
					fifo_rdreq <= '1';
				  if r.counter = TRACE_BURST_LEN-1 then
						v.csr.tbufwrite := r.csr.tbufwrite + TRACE_BURST_LEN*TRACE_BUS_WIDTH/8;
					  v.bus_state := st_bus_idle;
					else
						v.counter := r.counter + 1;
					end if;
				end if;			
		end case;

		fifo_trace <= trace;
		fifo_trace_len(fifo_trace_len'high downto 2) <= trace_len;
		dout.stall <= stall1; 
		next_r <= v;
	end process next_state_proc;	
	
	fifo_trace_len(1 downto 0) <= "00";
	
	process(reset, clk)
	begin
		if reset = '1' then
			r.state <= st_idle;

			r.csr.trace.wrapped <= '0';
			r.csr.trace.emittimestamp <= '0';
			r.csr.trace.emitstoredata <= '0';
			r.csr.trace.emitloaddata <= '0';
			r.csr.trace.emitstoreaddr <= '0';
			r.csr.trace.emitloadaddr <= '0';
			r.csr.trace.emitpriv <= '0';
			r.csr.trace.emitbranch <= '1';
			r.csr.trace.emitpc <= '1';
			r.csr.trace.fullaction <= "00";
			r.csr.trace.destination <= "01";  -- RAM on system bus enabled
			r.csr.trace.stall <= '1';
			r.csr.trace.discard <= '0';
			r.csr.tbufstart <= CONV_STD_LOGIC_VECTOR(INIT_BUF_START, WORD_SIZE);
			r.csr.tbufend <= CONV_STD_LOGIC_VECTOR(INIT_BUF_END, WORD_SIZE);
			r.csr.tbufwrite <= CONV_STD_LOGIC_VECTOR(INIT_BUF_START, WORD_SIZE);
			
			r.trace_in1.event <= trace_evt_none;
			r.trace_in2.event <= trace_evt_none;
			
		  r.pc_len <= (others => '0');
		  r.pc_data_len <= (others => '0');
		  r.branch_target_len <= (others => '0');
		  r.branch_target_data_len <= (others => '0');
		  r.address_len <= (others => '0');
		  r.readdata_len <= (others => '0');
		  r.writedata_len <= (others => '0');
		  r.timestamp_len	<= (others => '0');		
			
		  r.last_pc <= (others => '0');
		  r.last_address <= (others => '0');
		  r.last_timestamp <= (others => '0');
			
			r.bus_state <= st_bus_idle;
		elsif rising_edge(clk) then	
			r <= next_r;
		end if;
	end process;
end behaviour;
