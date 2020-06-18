-- Project Sorcerer - RISC-V core
--
-- Useful types & functions
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

package utils is											 
	type std_logic_2d_type is array(natural range <>, natural range <>) of std_logic;
	
  function log2(v: in natural) return natural;
	function ceil_log2(v: in natural) return natural;
	function port_bits(v: in natural) return natural;
  function decode(v : std_logic_vector) return std_logic_vector;
	-- c - decoded shift amount
	function shift_r(a: std_logic_vector; c: std_logic_vector) return std_logic_vector;
	-- c - conventional shift amount
	function barrel_shift_r(a: std_logic_vector; c: std_logic_vector; sign: std_logic) 
	    return std_logic_vector;
	function barrel_shift_r(a: std_logic_vector; c: std_logic_vector) return std_logic_vector;
	function barrel_shift_l(a: std_logic_vector; c: std_logic_vector) return std_logic_vector;
  function sign_extend(a: std_logic_vector; len: natural) return std_logic_vector;	
	function sign_extend32(a: std_logic_vector)  return std_logic_vector;
	function zero_extend(a: std_logic_vector; len: natural) return std_logic_vector;
	function imin(a: integer; b: integer) return integer;
	function imax(a: integer; b: integer) return integer;
	function to_std_logic(v: in integer) return std_logic;
end package utils;	

library IEEE;
use IEEE.STD_LOGIC_ARITH.all;

package body utils is
	function log2(v: in natural) return natural is
  	variable n: natural;
  	variable logn: natural;
  begin
  	n := 1;
  	for i in 0 to 128 loop
  		logn := i;
  		exit when (n>=v);
  		n := n * 2;
  	end loop;
		if v > 0 and n > v then 
			return logn - 1;
		else	
  	  return logn;
		end if;	
  end function log2;
	
	function ceil_log2(v: in natural) return natural is
  begin
		return log2(v - 1) + 1;
  end function ceil_log2; 	
  
	function port_bits(v: in natural) return natural is
	begin
		return log2(v - 1) + 1; 
	end function port_bits;	
	
  function decode(v : std_logic_vector) return std_logic_vector is
    variable res : std_logic_vector((2**v'length)-1 downto 0);
    variable i : natural;
  begin
    res := (others => '0');
    i := conv_integer(unsigned(v));
    res(i) := '1';
    return(res);
  end;
	
  function shift_r(a: std_logic_vector; c: std_logic_vector) 
	    return std_logic_vector	is
		variable n: natural;
		variable v: std_logic_vector(a'high downto a'low);
		variable w: natural;
	begin
		n := 1;
		v := a;
		w := port_bits(a'length);
		for i in c'low to c'low+ w- 1 loop
			if c(i) = '1' then
				for j in a'low to a'high loop
					if j+ n <= a'high then
						v(j) := v(j+ n);
					else
						v(j) := '0';
					end if;	
				end loop;
			end if;
			n := n * 2;
		end loop;		
		return v;
	end function shift_r;
	
	function barrel_shift_r(a: std_logic_vector; c: std_logic_vector; sign: std_logic) 
	    return std_logic_vector is
	  variable v: std_logic_vector(imax(a'length, 2**c'length)-1 downto 0);
		variable sh: integer;
		variable fill: std_logic_vector(v'high downto 0);
	begin
		if sign = '1' and a(a'high) = '1' then
			fill := (others => '1');
			v := (others => '1');
		else
			fill := (others => '0');
			v := (others => '0');
		end if;	
		v(a'range) := a;
		sh := 1;
		for i in c'low to c'high loop
			if c(i) = '1' then 
				for k in 0 to v'high-sh loop
					v(k) := v(k + sh);
				end loop;
				v(v'high downto v'high-sh+1) := fill(v'high downto v'high-sh+1);
			end if;	
			sh := sh * 2;
		end loop;
		return v(a'range);
	end function barrel_shift_r;
	
	function barrel_shift_r(a: std_logic_vector; c: std_logic_vector)  return std_logic_vector is
	begin
		return barrel_shift_r(a, c, '0');
	end function barrel_shift_r;	
	
	function barrel_shift_l(a: std_logic_vector; c: std_logic_vector) return std_logic_vector is
	  variable v: std_logic_vector(a'length+imax(a'length,2**c'length)-1 downto 0);
		variable sh: integer;
	begin
		v := (others => '0');
		v(a'range) := a;
		sh := 1;
		for i in c'low to c'high loop
			if c(i) = '1' then 
				for k in v'high-sh downto 0 loop
					v(k + sh) := v(k);
				end loop;
				v(sh-1 downto 0) := (others => '0');
			end if;	
			sh := sh * 2;
		end loop;
		return v;
	end function barrel_shift_l;	
	
	function sign_extend(a: std_logic_vector; len: natural)  return std_logic_vector is
	  variable v: std_logic_vector(len-1 downto 0);
	begin							
		for i in 0 to a'high loop
		  v(i) := a(i);
		end loop;
		if a(a'high) = '1' then
			for i in a'high+1 to len-1 loop
				v(i) := '1';
			end loop;	
		else	
			for i in a'high+1 to len-1 loop
				v(i) := '0';
			end loop;			
		end if;	
		return v;
	end function sign_extend;
	
	function sign_extend32(a: std_logic_vector)  return std_logic_vector is
	begin							
		return sign_extend(a, 32);
	end function sign_extend32;		

  function zero_extend(a: std_logic_vector; len: natural) return std_logic_vector is
	  variable v: std_logic_vector(len-1 downto 0);
	begin
		v(a'range) := a;
		if v'high > a'high then
			v(v'high downto a'high+1) := (others => '0');
		end if;	
		return v;
	end function zero_extend;	
	
  function imin(a: integer; b: integer) return integer	is
	begin
		if a < b then
			return a;
		else	
			return b;
		end if;	
	end function imin;	
	
  function imax(a: integer; b: integer) return integer	is
	begin
		if a > b then
			return a;
		else	
			return b;
		end if;	
	end function imax;
	
	function to_std_logic(v: in integer) return std_logic is
	begin
		case v is
			when 0 => return '0';
			when 1 => return '1';
			when others => return 'X';
		end case;	
	end function to_std_logic;	
end package body utils;
