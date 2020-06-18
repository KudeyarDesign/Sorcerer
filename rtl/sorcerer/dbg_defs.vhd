-- Project Sorcerer - RISC-V core
--
-- Constants and Functions for Debugger
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

package dbg_defs is	
  constant MATCH_JAL: natural := 16#6f#; 
	constant MATCH_ADDI: natural := 16#13#;
	constant MATCH_ORI: natural := 16#6013#;
	constant MATCH_XORI: natural := 16#4013#;
	constant MATCH_SRLI: natural := 16#5013#;
  constant MATCH_LB: natural := 16#3#;
  constant MATCH_LH: natural := 16#1003#;
  constant MATCH_LW: natural := 16#2003#;
  constant MATCH_LBU: natural := 16#4003#;
  constant MATCH_LHU: natural := 16#5003#;
  constant MATCH_SB: natural := 16#23#;
  constant MATCH_SH: natural := 16#1023#;
  constant MATCH_SW: natural := 16#2023#;
  constant MATCH_FENCE_I: natural := 16#100f#;
  constant MATCH_CSRRW: natural := 16#1073#;
  constant MATCH_CSRRS: natural := 16#2073#;
  constant MATCH_CSRRC: natural := 16#3073#;
  constant MATCH_CSRRWI: natural := 16#5073#;
  constant MATCH_CSRRSI: natural := 16#6073#;
  constant MATCH_CSRRCI: natural := 16#7073#;			

  function bits(value: natural; hi: natural; lo: natural) return std_logic_vector;
	function bit1(value: natural; b: natural)  return std_logic_vector;
	
  function jal(rd: natural range 0 to 31; imm: std_logic_vector) 
	  return std_logic_vector;
  function csrsi(csr: natural range 0 to 4095; imm: std_logic_vector)
	  return std_logic_vector;
  function csrsi(csr: std_logic_vector(11 downto 0); imm: std_logic_vector)
	  return std_logic_vector;	
  function csrci(csr: natural range 0 to 4095; imm: std_logic_vector)
	  return std_logic_vector;
  function csrci(csr: std_logic_vector(11 downto 0); imm: std_logic_vector)
	  return std_logic_vector;	
  function csrr(rd: natural range 0 to 31; csr: natural range 0 to 4095)
	  return std_logic_vector;
  function csrr(rd: natural range 0 to 31; csr: std_logic_vector(11 downto 0))
	  return std_logic_vector;	
  function csrw(rs: natural range 0 to 31; csr: natural range 0 to 4095)
	  return std_logic_vector;
  function csrw(rs: natural range 0 to 31;  csr: std_logic_vector(11 downto 0))
	  return std_logic_vector;	
  function fence_i return std_logic_vector;
  function lb(rd: natural range 0 to 31; base: natural range 0 to 31; offset: natural)
	    return std_logic_vector;
  function lh(rd: natural range 0 to 31; base: natural range 0 to 31; offset: natural)
	    return std_logic_vector;
  function lw(rd: natural range 0 to 31; base: natural range 0 to 31; offset: natural)
	    return std_logic_vector;
  function lw(rd: natural range 0 to 31; base: natural range 0 to 31; offset: std_logic_vector)
	    return std_logic_vector;
  function sb(src: natural range 0 to 31; base: natural range 0 to 31; offset: natural)
	    return std_logic_vector;	
  function sh(src: natural range 0 to 31; base: natural range 0 to 31; offset: natural)
	    return std_logic_vector;	
  function sw(src: natural range 0 to 31; base: natural range 0 to 31; offset: natural)
	    return std_logic_vector;
  function sw(src: natural range 0 to 31; base: natural range 0 to 31; offset: std_logic_vector)
	    return std_logic_vector;
  function addi(dest: natural range 0 to 31; src: natural range 0 to 31; imm: natural)
	    return std_logic_vector;	
  function ori(dest: natural range 0 to 31; src: natural range 0 to 31; imm: natural)
	    return std_logic_vector;			
  function xori(dest: natural range 0 to 31; src: natural range 0 to 31; imm: natural)
	    return std_logic_vector;			
  function srli(dest: natural range 0 to 31; src: natural range 0 to 31; shamt: natural range 0 to 31)
	    return std_logic_vector;			
  function nop return std_logic_vector;
end package dbg_defs;

package body dbg_defs is
  function bits(value: natural; hi: natural; lo: natural) return std_logic_vector is
	  variable v,v2: std_logic_vector(31 downto 0);
	begin								
		v := CONV_STD_LOGIC_VECTOR(value, 32);	 
		v2 := (others => '0');
		v2(hi-lo downto 0) := v(hi downto lo);
		return v2;
	end function bits;	

  function bit1(value: natural; b: natural)  return std_logic_vector is
	  variable v: std_logic_vector(31 downto 0);
	begin							 
		v := CONV_STD_LOGIC_VECTOR(value, 32);
		if v(b) = '1' then
		  return CONV_STD_LOGIC_VECTOR(1, 32);
		else
			return CONV_STD_LOGIC_VECTOR(0, 32);
		end if;
	end function bit1;	
	
	function lsl(value: std_logic; b: natural) return std_logic_vector is
	  variable v: std_logic_vector(31 downto 0);
	begin
		v := (others => '0');
		v(b) := value;
		return v;
	end function lsl;	
	
	function lsl(value: std_logic_vector; b: natural) return std_logic_vector is
	  variable v: std_logic_vector(63 downto 0);
	begin
		v := (others => '0');
		v(b+value'length-1 downto b) := value;
		return v(31 downto 0);
	end function lsl;
	
	function lsl(value: natural; b: natural) return std_logic_vector is
	begin
		return lsl(CONV_STD_LOGIC_VECTOR(value, 32), b);
	end function lsl;	
	
  function jal(rd: natural range 0 to 31; imm: std_logic_vector) 
	    return std_logic_vector is
	begin
    return lsl(imm(20), 31) or
		       lsl(imm(10 downto 1), 21) or
					 lsl(imm(11), 20) or
					 lsl(imm(19 downto 12), 12) or
					 CONV_STD_LOGIC_VECTOR(MATCH_JAL, 32);
	end function jal;	
	
  function csrsi(csr: natural range 0 to 4095; imm: std_logic_vector)
	    return std_logic_vector is
	begin
    return lsl(CONV_STD_LOGIC_VECTOR(csr, 12), 20) or
           lsl(imm(4 downto 0), 15) or
           CONV_STD_LOGIC_VECTOR(MATCH_CSRRSI, 32);
	end function csrsi;
	
  function csrsi(csr: std_logic_vector(11 downto 0); imm: std_logic_vector)
	    return std_logic_vector is
	begin
    return csrsi(CONV_INTEGER(csr), imm);
	end function csrsi;	
	
  function csrci(csr: natural range 0 to 4095; imm: std_logic_vector)
	    return std_logic_vector is
	begin
    return lsl(CONV_STD_LOGIC_VECTOR(csr, 12), 20) or
           lsl(imm(4 downto 0), 15) or
           CONV_STD_LOGIC_VECTOR(MATCH_CSRRCI, 32);
	end function csrci;
	
  function csrci(csr: std_logic_vector(11 downto 0); imm: std_logic_vector)
	    return std_logic_vector is
	begin
    return csrsi(CONV_INTEGER(csr), imm);
	end function csrci;	
	
  function csrr(rd: natural range 0 to 31; csr: natural range 0 to 4095)
	    return std_logic_vector is
	begin
    return lsl(CONV_STD_LOGIC_VECTOR(csr, 12), 20) or
           lsl(CONV_STD_LOGIC_VECTOR(rd, 5), 7) or
           CONV_STD_LOGIC_VECTOR(MATCH_CSRRS, 32);
	end function csrr;
	
  function csrr(rd: natural range 0 to 31; csr: std_logic_vector(11 downto 0))
	    return std_logic_vector is
	begin
    return csrr(rd, CONV_INTEGER(csr));
	end function csrr;	
	
  function csrw(rs: natural range 0 to 31; csr: natural range 0 to 4095)
	    return std_logic_vector is
	begin
    return lsl(CONV_STD_LOGIC_VECTOR(csr, 12), 20) or
           lsl(rs, 15) or
           CONV_STD_LOGIC_VECTOR(MATCH_CSRRW, 32);
	end function csrw;	
	
  function csrw(rs: natural range 0 to 31; csr: std_logic_vector(11 downto 0))
	    return std_logic_vector is
	begin
    return csrw(rs, CONV_INTEGER(csr));
	end function csrw;	
	
  function fence_i return std_logic_vector is
	begin
    return CONV_STD_LOGIC_VECTOR(MATCH_FENCE_I, 32);
	end function fence_i;	
	
  function sb(src: natural range 0 to 31; base: natural range 0 to 31; offset: natural)
	    return std_logic_vector is
	begin
		return lsl(bits(offset, 11, 5), 25) or
		       lsl(src, 20) or
		       lsl(base, 15) or
		       lsl(bits(offset, 4, 0), 7) or
		       CONV_STD_LOGIC_VECTOR(MATCH_SB, 32);
	end function sb;	
	
  function sh(src: natural range 0 to 31; base: natural range 0 to 31; offset: natural)
	    return std_logic_vector is
	begin
		return lsl(bits(offset, 11, 5), 25) or
		       lsl(src, 20) or
		       lsl(base, 15) or
		       lsl(bits(offset, 4, 0), 7) or
		       CONV_STD_LOGIC_VECTOR(MATCH_SH, 32);
	end function sh;
	
  function sw(src: natural range 0 to 31; base: natural range 0 to 31; offset: natural)
	    return std_logic_vector is
	begin
		return lsl(bits(offset, 11, 5), 25) or
		       lsl(src, 20) or
		       lsl(base, 15) or
		       lsl(bits(offset, 4, 0), 7) or
		       CONV_STD_LOGIC_VECTOR(MATCH_SW, 32);
	end function sw;
	
  function sw(src: natural range 0 to 31; base: natural range 0 to 31; offset: std_logic_vector)
	    return std_logic_vector is
	begin
		return lsl(offset(11 downto 5), 25) or
		       lsl(src, 20) or
		       lsl(base, 15) or
		       lsl(offset(4 downto 0), 7) or
		       CONV_STD_LOGIC_VECTOR(MATCH_SW, 32);
	end function sw;	
	
  function lb(rd: natural range 0 to 31; base: natural range 0 to 31; offset: natural)
	    return std_logic_vector is
	begin
		return lsl(bits(offset, 11, 0), 20) or
		       lsl(base, 15) or
		       lsl(bits(rd, 4, 0), 7) or
		       CONV_STD_LOGIC_VECTOR(MATCH_LB, 32);
	end function lb;
	
  function lh(rd: natural range 0 to 31; base: natural range 0 to 31; offset: natural)
	    return std_logic_vector is
	begin
		return lsl(bits(offset, 11, 0), 20) or
		       lsl(base, 15) or
		       lsl(bits(rd, 4, 0), 7) or
		       CONV_STD_LOGIC_VECTOR(MATCH_LH, 32);
	end function lh;	
	
  function lw(rd: natural range 0 to 31; base: natural range 0 to 31; offset: std_logic_vector)
	    return std_logic_vector is
	begin
		return lsl(offset(11 downto 0), 20) or
		       lsl(base, 15) or
		       lsl(bits(rd, 4, 0), 7) or
		       CONV_STD_LOGIC_VECTOR(MATCH_LW, 32);
	end function lw;	
	
  function lw(rd: natural range 0 to 31; base: natural range 0 to 31; offset: natural)
	    return std_logic_vector is
	begin
		return lsl(bits(offset, 11, 0), 20) or
		       lsl(base, 15) or
		       lsl(bits(rd, 4, 0), 7) or
		       CONV_STD_LOGIC_VECTOR(MATCH_LW, 32);
	end function lw;
	
  function addi(dest: natural range 0 to 31; src: natural range 0 to 31; imm: natural)
	    return std_logic_vector is
	begin
		return lsl(bits(imm, 11, 0), 20) or
		       lsl(src, 15) or
		  		 lsl(dest, 7) or
					 CONV_STD_LOGIC_VECTOR(MATCH_ADDI, 32);
	end function addi;	

  function ori(dest: natural range 0 to 31; src: natural range 0 to 31; imm: natural)
	    return std_logic_vector is
	begin
		return lsl(bits(imm, 11, 0), 20) or
		       lsl(src, 15) or
		  		 lsl(dest, 7) or
					 CONV_STD_LOGIC_VECTOR(MATCH_ORI, 32);
	end function ori;
	
  function xori(dest: natural range 0 to 31; src: natural range 0 to 31; imm: natural)
	    return std_logic_vector is
	begin
		return lsl(bits(imm, 11, 0), 20) or
		       lsl(src, 15) or
		  		 lsl(dest, 7) or
					 CONV_STD_LOGIC_VECTOR(MATCH_XORI, 32);
	end function xori;	

  function srli(dest: natural range 0 to 31; src: natural range 0 to 31; shamt: natural range 0 to 31)
	    return std_logic_vector is
	begin
		return lsl(bits(shamt, 11, 0), 20) or
		       lsl(src, 15) or
		  		 lsl(dest, 7) or
					 CONV_STD_LOGIC_VECTOR(MATCH_SRLI, 32);		
	end function srli;	

  function nop return std_logic_vector is
  begin
   return addi(0, 0, 0);
  end function nop;	
	
end dbg_defs;
