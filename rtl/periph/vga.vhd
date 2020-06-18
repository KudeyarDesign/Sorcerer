-- Project Sorcerer - RISC-V core
--
-- VGA Display Module for Avalon MM Bus
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

entity vga is
	generic
	(	 
	  DISPLAY_BUF_START: natural := 0;
	  DEVICE_FAMILY: string := "Cyclone V";	
		WIDTH: natural range 16 to 128 := 32;
		FIFO_DEPTH: natural range 16 to 8192 := 128;
		BURST_LEN: natural range 4 to 64 := 8;
		CLK_PRESCALE: natural range 1 to 8 := 2
	);
  port
  (
		bus_reset: in std_logic;
		bus_clk:   in std_logic;
		pix_reset: in std_logic;
		pix_clk: in std_logic;
		
		avm_mem_address: out std_logic_vector(31 downto 0);
		avm_mem_burstcount: out std_logic_vector(log2(BURST_LEN) downto 0);
		avm_mem_read: out std_logic;
		avm_mem_readdata: in std_logic_vector(31 downto 0);
		avm_mem_readdatavalid: in std_logic;
		avm_mem_waitrequest: in std_logic;		

		RED: out std_logic_vector(7 downto 0);
		GREEN: out std_logic_vector(7 downto 0);
		BLUE:  out std_logic_vector(7 downto 0);
		nRAMDAC_BLANK: out std_logic;
		nRAMDAC_SYNC: out std_logic;
		RAMDAC_CLK: out std_logic;
		HSYNC: out std_logic;
		VSYNC: out std_logic
  );	
end vga;

architecture behaviour of vga is 
	constant RGB16_RED_WIDTH: natural:=	5;
	constant RGB16_GREEN_WIDTH: natural:= 6;
	constant RGB16_BLUE_WIDTH: natural:= 5;
	constant RGB16_BLUE_OFFSET: natural:=	0;
	constant RGB16_GREEN_OFFSET: natural:=	RGB16_BLUE_WIDTH;
	constant RGB16_RED_OFFSET: natural:= (RGB16_BLUE_WIDTH+ RGB16_GREEN_WIDTH);

  constant DISPLAY_START: std_logic_vector(31 downto 0) := CONV_STD_LOGIC_VECTOR(DISPLAY_BUF_START, 32);

  component display_fifo is
	generic
	(
	  WIDTH: natural range 8 to 128 := 32;
		DEPTH: natural range 16 to 8192 := 128;
		DEVICE_FAMILY: string := "Cyclone V"
	); 
  port
  (
		data		: in std_logic_vector(WIDTH-1 downto 0);
		wrreq		: in std_logic;
		rdreq		: in std_logic;
		rdclk		: in std_logic;
		wrclk		: in std_logic;
		aclr		: in std_logic;
		q		: out std_logic_vector(WIDTH-1 downto 0);
		rdempty		: out std_logic;
		wrusedw		: out std_logic_vector(ceil_log2(DEPTH)-1 downto 0)
	);	
  end component;

	type VGA_timings is 
	record
		h_polarity: boolean;
	  h_visible_area: natural;
		h_front_porch: natural;
		h_sync_pulse: natural;
		h_back_porch: natural;
		whole_line: natural;	
		v_polarity: boolean;
	  v_visible_area: natural;
		v_front_porch: natural;
		v_sync_pulse: natural;
		v_back_porch: natural;
		whole_frame: natural;
	end record;

		-- 640 x 480 x 60 Hz , f = 25.175 Mhz	Industry standard timing
	constant VGA_640x480x60: VGA_timings :=
	(			
	  h_polarity => false,    -- negative
	  h_visible_area => 640,	-- 25.422045680238 us
		h_front_porch => 16,		-- 0.63555114200596 us
		h_sync_pulse => 96,			-- 3.8133068520357 us 
		h_back_porch => 48,		  -- 1.9066534260179 us
		whole_line => 800,	 		-- 31.777557100298 us
		v_polarity => false,    -- negative
		v_visible_area => 480,	-- 15.253227408143 ms
		v_front_porch => 10,		-- 0.31777557100298 ms
		v_sync_pulse => 2,			-- 0.063555114200596 ms
		v_back_porch => 33,			-- 1.0486593843098 ms
		whole_frame => 525			-- 16.683217477656 ms
	);	

  type internal_timings is 
  record
		h_sync_inv: std_logic;
  	h_sync_end: std_logic_vector(10 downto 0);
	  h_blank_start: std_logic_vector(10 downto 0);
	  h_blank_end: std_logic_vector(10 downto 0);
	  h_size: std_logic_vector(10 downto 0);
		v_sync_inv: std_logic;
    v_sync_end: std_logic_vector(10 downto 0);
	  v_blank_start: std_logic_vector(10 downto 0);
	  v_blank_end: std_logic_vector(10 downto 0);
	  v_size: std_logic_vector(10 downto 0);
		pixel_num: std_logic_vector(21 downto 0);
	end record;
	
	function convert_timings(t: VGA_timings) return internal_timings is
	  variable v: internal_timings;
	begin																	
		assert t.h_visible_area + t.h_front_porch+ t.h_sync_pulse+ t.h_back_porch = t.whole_line 
		  report "Invalid horizontal timings" severity error;	
		assert t.v_visible_area + t.v_front_porch+ t.v_sync_pulse+ t.v_back_porch = t.whole_frame 
		  report "Invalid vertical timings" severity error;		
		if t.h_polarity then
			v.h_sync_inv := '0';
		else
			v.h_sync_inv := '1';
		end if;	
		v.h_sync_end := conv_std_logic_vector(t.h_sync_pulse- 1, 11);
		v.h_blank_start := conv_std_logic_vector(t.whole_line- t.h_front_porch- 1, 11);
		v.h_blank_end := conv_std_logic_vector(t.h_sync_pulse+ t.h_back_porch- 1, 11);
	  v.h_size := conv_std_logic_vector(t.whole_line- 1, 11);
		if t.v_polarity then
			v.v_sync_inv := '0';
		else
			v.v_sync_inv := '1';
		end if;		
		v.v_sync_end := conv_std_logic_vector(t.v_sync_pulse- 1, 11);
		v.v_blank_start := conv_std_logic_vector(t.whole_frame- t.v_front_porch- 1, 11);
		v.v_blank_end := conv_std_logic_vector(t.v_sync_pulse+ t.v_back_porch- 1, 11);
		v.v_size := conv_std_logic_vector(t.whole_frame- 1, 11);
		v.pixel_num := conv_std_logic_vector(t.h_visible_area * t.v_visible_area, v.pixel_num'length);
		return v;
	end function;	
	
  constant tms: internal_timings := convert_timings(VGA_640x480x60);	
	
	type reg_type is record	 
		prescale_cnt: std_logic_vector(3 downto 0);
		pixel_num: std_logic_vector(0 downto 0);
	  hcnt: std_logic_vector(11 downto 0);
		hblank: std_logic; 
		hblank_del: std_logic_vector(1 to 4);
		hsync: std_logic;
	  vcnt: std_logic_vector(11 downto 0);
		vblank: std_logic;
		vsync: std_logic;
	  red: std_logic_vector(7 downto 0);
	  green: std_logic_vector(7 downto 0);
	  blue: std_logic_vector(7 downto 0);		
	end record;	

	signal r, next_r: reg_type;	
	
	signal fifo_data: std_logic_vector(WIDTH-1 downto 0);
	signal fifo_wrreq: std_logic;
	signal fifo_rdreq: std_logic;
	signal fifo_rdclk: std_logic;
	signal fifo_aclr: std_logic;
	signal fifo_q: std_logic_vector(WIDTH-1 downto 0);
	signal fifo_rdempty: std_logic;
	signal fifo_wrusedw: std_logic_vector(ceil_log2(FIFO_DEPTH)-1 downto 0);
	
	type bus_state_type is (st_idle, st_wait_fifo, st_request_read, st_read);
	type bus_reg_type is record
	  state: bus_state_type; 
		address: std_logic_vector(31 downto 0);
		word_cnt: std_logic_vector(20 downto 0);
	  burst_cnt: std_logic_vector(log2(BURST_LEN)-1 downto 0);
	end record;
	
	signal br, next_br: bus_reg_type;
	
	signal frame_start: std_logic;
	signal frame_end: std_logic;
	
	signal cross_sync: std_logic_vector(2 downto 0);
begin
  id_fifo: display_fifo
	generic map
	(
	  WIDTH => WIDTH,
		DEPTH => FIFO_DEPTH,
		DEVICE_FAMILY => DEVICE_FAMILY
	)	
	port map
	(
	  data => fifo_data,
	  wrreq	=> fifo_wrreq,
	  rdreq	=> fifo_rdreq,
	  rdclk	=> pix_clk,
	  wrclk	=> bus_clk,
	  aclr	=> fifo_aclr,
	  q	=> fifo_q,
	  rdempty => fifo_rdempty,
	  wrusedw	=> fifo_wrusedw		
	);	

	cross_sync_proc: process(bus_reset, bus_clk)
	begin
		if bus_reset = '1' then
			cross_sync <= (others => '0');
	  elsif rising_edge(bus_clk) then		
			for i in 1 to cross_sync'high loop
				cross_sync(i) <= cross_sync(i-1);
			end loop;	
			cross_sync(0) <= r.vsync;
		end if;	
	end process cross_sync_proc;	
	
	frame_start <= cross_sync(cross_sync'high-1) and not cross_sync(cross_sync'high);
	frame_end <= '1' when br.word_cnt >= tms.pixel_num(br.word_cnt'high downto 1)
	  else '0';
	fifo_data <= avm_mem_readdata;	
	fifo_aclr <= bus_reset or frame_start;
	
	bus_next_state: process(br, frame_start, frame_end, fifo_wrusedw, 
	    avm_mem_waitrequest, avm_mem_readdatavalid)
	  variable v: bus_reg_type;
		variable offset: std_logic_vector(31 downto 0);
	begin
		v := br;
		
		avm_mem_read <= '0';
		avm_mem_burstcount <= (others => '0');
		fifo_wrreq <= '0';
		case br.state is
			when st_idle =>
			  if frame_start = '1' then	
				  v.word_cnt := (others => '0'); 
					v.state := st_wait_fifo;
				end if;	
			when st_wait_fifo => 
			if fifo_wrusedw(fifo_wrusedw'high) = '0' then
				 offset := (others => '0');
				 offset(br.word_cnt'high+log2(WIDTH/8) downto log2(BURST_LEN*WIDTH/8)) := br.word_cnt(br.word_cnt'high downto log2(BURST_LEN)); 
				 v.address := DISPLAY_START + offset;
				 v.state := st_request_read;
				end if;
				v.burst_cnt := (others => '1');
			when st_request_read =>	
			  avm_mem_read <= '1';
				avm_mem_burstcount <= CONV_STD_LOGIC_VECTOR(BURST_LEN, avm_mem_burstcount'length);
				if avm_mem_waitrequest = '0' then
					v.state := st_read;
				end if;			
			when st_read =>
			  if avm_mem_readdatavalid = '1' then
				  fifo_wrreq <= '1';
					if br.burst_cnt = 0 then
						if frame_end = '1' then
							v.state := st_idle;
						else	
						  v.state := st_wait_fifo;
						end if;	
					end if;	
					v.burst_cnt := br.burst_cnt - 1;
					v.word_cnt := v.word_cnt + 1;
				end if;			
		end case;	
		
		avm_mem_address <= br.address;
		next_br <= v;
	end process bus_next_state;	
	
	process(bus_reset, bus_clk)
	begin
		if bus_reset = '1' then
			br.state <= st_idle;
			br.word_cnt <= (others => '0');
			br.burst_cnt <= (others => '0');
		elsif rising_edge(bus_clk) then	
			br <= next_br;
		end if;
	end process;	
	
	next_state: process(r)
	  variable v: reg_type;	
		variable color16: std_logic_vector(15 downto 0);
	begin
		v := r;
		fifo_rdreq <= '0';
		
		if r.prescale_cnt = CLK_PRESCALE-1 then
			v.prescale_cnt := (others => '0');
			if r.hcnt = tms.h_sync_end then
				v.hsync := '0';	
			end if;	
			if r.hcnt = tms.h_blank_start then
				v.hblank := '1';
			elsif r.hcnt = tms.h_blank_end then
				v.hblank := '0';
			end if;	
	    if r.hcnt = tms.h_size then
				v.hcnt := (others => '0');
				v.hsync := '1';			
				
				if r.vcnt = tms.v_sync_end then
					v.vsync := '0';	
				end if;	
				if r.vcnt = tms.v_blank_start then
					v.vblank := '1';
				elsif r.vcnt = tms.v_blank_end then
					v.vblank := '0';
				end if;	
		    if r.vcnt = tms.v_size then
					v.vcnt := (others => '0');
					v.vsync :='1';
				else	
		      v.vcnt := r.vcnt + 1;
		    end if;	
			else	
	      v.hcnt := r.hcnt + 1;
	    end if;	
			
		  if r.pixel_num(0) = '1' then
				color16 := fifo_q(31 downto 16);
			else	
				color16 := fifo_q(15 downto 0);
			end if;
		  v.red(7 downto 8- RGB16_RED_WIDTH) := 
			  color16(RGB16_RED_OFFSET + RGB16_RED_WIDTH - 1 downto RGB16_RED_OFFSET);
		  v.red(7- RGB16_RED_WIDTH downto 0) := (others => '0');	
		  v.green(7 downto 8- RGB16_GREEN_WIDTH) := 
			  color16(RGB16_GREEN_OFFSET + RGB16_GREEN_WIDTH - 1 downto RGB16_GREEN_OFFSET);	
			v.green(7- RGB16_GREEN_WIDTH downto 0) := (others => '0');
		  v.blue(7 downto 8- RGB16_BLUE_WIDTH) := 
			  color16(RGB16_BLUE_OFFSET + RGB16_BLUE_WIDTH - 1 downto RGB16_BLUE_OFFSET);	
      v.blue(7- RGB16_BLUE_WIDTH downto 0) := (others => '0');		
			
			v.pixel_num := r.pixel_num + 1;
			
			if r.hblank = '0' and r.vblank = '0' and r.pixel_num = 1 then
				fifo_rdreq <= '1';
			end if;	
			for i in 2 to r.hblank_del'high loop
				v.hblank_del(i) := r.hblank_del(i - 1);
			end loop;	
			v.hblank_del(1) := r.hblank;			
		else	
			v.prescale_cnt := r.prescale_cnt + 1;			
		end if;  -- r.prescale_cnt
		

		
		next_r <= v;
	end process;	
	
  process(pix_reset, pix_clk)
	begin
		if pix_reset = '1' then	
			r.prescale_cnt <= (others => '0');
			r.pixel_num <= (others => '0');
			r.hcnt <= (others => '0');
			r.hblank <= '0';
			r.hblank_del <= (others => '0');
			r.hsync <= '0';	
			r.vcnt <= CONV_STD_LOGIC_VECTOR(470, r.hcnt'length); -- (others => '0');
			r.vblank <= '0';
			r.vsync <= '0';
			r.red <= (others => '0');
			r.green <= (others => '0');
			r.blue <= (others => '0');
		elsif rising_edge(pix_clk) then	
			r <= next_r;			
		end if;	
	end process;
	
	RED <= r.red;
	GREEN <= r.green;
	BLUE <= r.blue;
	
  nRAMDAC_BLANK <= not (r.hblank_del(3) or r.vblank); 
  nRAMDAC_SYNC <= '0';
  RAMDAC_CLK <= not pix_clk;	
	
	HSYNC <= r.hsync xor tms.h_sync_inv;
	VSYNC <= r.vsync xor tms.v_sync_inv;	
end behaviour;
