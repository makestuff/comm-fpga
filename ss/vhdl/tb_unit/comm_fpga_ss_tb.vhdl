--
-- Copyright (C) 2009-2012 Chris McClelland
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_textio.all;
use std.textio.all;

entity comm_fpga_ss_tb is
end entity;

architecture behavioural of comm_fpga_ss_tb is
	-- Clocks
	signal sysClk     : std_logic;  -- main system clock
	signal dispClk    : std_logic;  -- display version of sysClk, which leads it by 4ns

	-- Serial signals
	signal serClk     : std_logic;
	signal serDataIn  : std_logic;
	signal serDataOut : std_logic;

	-- Pipe signals
	signal chanAddr   : std_logic_vector(6 downto 0);

	signal h2fData    : std_logic_vector(7 downto 0);
	signal h2fValid   : std_logic;
	signal h2fReady   : std_logic;

	signal f2hData    : std_logic_vector(7 downto 0);
	signal f2hValid   : std_logic;
	signal f2hReady   : std_logic;
	
	-- Pause for N serClks
	procedure pause(constant n : in integer) is
		variable i : integer;
	begin
		for i in 1 to n loop
			wait until rising_edge(serClk);
		end loop;
	end procedure;
begin
	-- Instantiate comm_fpga_ss for testing
	uut: entity work.comm_fpga_ss
		port map(
			clk_in        => sysClk,

			-- Serial interface
			serClk_in     => serClk,
			serData_in    => serDataIn,
			serData_out   => serDataOut,

			-- Channel read/write
			chanAddr_out => chanAddr,
			
			h2fData_out  => h2fData,
			h2fValid_out => h2fValid,
			h2fReady_in  => h2fReady,

			-- Data to send
			f2hData_in   => f2hData,
			f2hValid_in  => f2hValid,
			f2hReady_out => f2hReady
		);

	-- Drive the clocks. In simulation, sysClk lags 4ns behind dispClk, to give a visual hold time
	-- for signals in GTKWave.
	process
	begin
		sysClk <= '0';
		dispClk <= '1';
		wait for 10 ns;
		dispClk <= '0';
		wait for 10 ns;
		loop
			dispClk <= '1';
			wait for 4 ns;
			sysClk <= '1';
			wait for 6 ns;
			dispClk <= '0';
			wait for 4 ns;
			sysClk <= '0';
			wait for 6 ns;
		end loop;
	end process;

	-- Drive serClk
	process
	begin
		serClk <= '0';
		loop
			wait until rising_edge(sysClk);
			wait until rising_edge(sysClk);
			wait until rising_edge(sysClk);
			wait until rising_edge(sysClk);
			serClk <= not(serClk);
		end loop;
	end process;

	-- Drive the sync serial side
	process
		procedure sendByte(constant b : in std_logic_vector(7 downto 0)) is
		begin
			serDataIn <= '0';   -- start bit
			wait until rising_edge(serClk); serDataIn <= b(0);  -- bit 0
			wait until rising_edge(serClk); serDataIn <= b(1);  -- bit 1
			wait until rising_edge(serClk); serDataIn <= b(2);  -- bit 2
			wait until rising_edge(serClk); serDataIn <= b(3);  -- bit 3
			wait until rising_edge(serClk); serDataIn <= b(4);  -- bit 4
			wait until rising_edge(serClk); serDataIn <= b(5);  -- bit 5
			wait until rising_edge(serClk); serDataIn <= b(6);  -- bit 6
			wait until rising_edge(serClk); serDataIn <= b(7);  -- bit 7
			wait until rising_edge(serClk); serDataIn <= '1';   -- stop bit
			wait until rising_edge(serClk);
		end procedure;
	begin
		serDataIn <= '1';
		pause(4);

		-- Send first packet (write 63 bytes)
		if ( serDataOut = '1' ) then
			wait until falling_edge(serDataOut);
			wait until rising_edge(serClk);
		end if;
		sendByte(x"00");  -- dir bit & channel
		sendByte(x"3F");  -- length
		sendByte(x"00"); sendByte(x"01"); sendByte(x"02"); sendByte(x"03"); sendByte(x"04"); sendByte(x"05"); sendByte(x"06"); sendByte(x"07"); sendByte(x"08"); sendByte(x"09"); sendByte(x"0A"); sendByte(x"0B"); sendByte(x"0C"); sendByte(x"0D"); sendByte(x"0E"); sendByte(x"0F");
		sendByte(x"10"); sendByte(x"11"); sendByte(x"12"); sendByte(x"13"); sendByte(x"14"); sendByte(x"15"); sendByte(x"16"); sendByte(x"17"); sendByte(x"18"); sendByte(x"19"); sendByte(x"1A"); sendByte(x"1B"); sendByte(x"1C"); sendByte(x"1D"); sendByte(x"1E"); sendByte(x"1F");
		sendByte(x"20"); sendByte(x"21"); sendByte(x"22"); sendByte(x"23"); sendByte(x"24"); sendByte(x"25"); sendByte(x"26"); sendByte(x"27"); sendByte(x"28"); sendByte(x"29"); sendByte(x"2A"); sendByte(x"2B"); sendByte(x"2C"); sendByte(x"2D"); sendByte(x"2E"); sendByte(x"2F");
		sendByte(x"30"); sendByte(x"31"); sendByte(x"32"); sendByte(x"33"); sendByte(x"34"); sendByte(x"35"); sendByte(x"36"); sendByte(x"37"); sendByte(x"38"); sendByte(x"39"); sendByte(x"3A"); sendByte(x"3B"); sendByte(x"3C"); sendByte(x"3D"); sendByte(x"3E");

		-- Send second packet (write four bytes)
		if ( serDataOut = '1' ) then
			wait until falling_edge(serDataOut);
			wait until rising_edge(serClk);
		end if;
		sendByte(x"00");
		sendByte(x"04");
		sendByte(x"55");sendByte(x"50");sendByte(x"AA");sendByte(x"A0");

		-- Send third packet (read four bytes)
		if ( serDataOut = '1' ) then
			wait until falling_edge(serDataOut);
			wait until rising_edge(serClk);
		end if;
		sendByte(x"80");
		sendByte(x"04");
		serDataIn <= '0';  -- "I'm ready to receive"
		pause(4*8+8);
		serDataIn <= '1';  -- "I've finished receiving"
		wait;
	end process;

	-- Drive the FPGA->Host pipe
	process
	begin
		f2hData <= (others => 'X');
		f2hValid <= '0';

		wait until rising_edge(f2hReady);
		pause(4);
		f2hData <= x"55";
		f2hValid <= '1';
		wait until falling_edge(f2hReady);
		f2hData <= (others => 'X');
		f2hValid <= '0';

		wait until rising_edge(f2hReady);
		f2hData <= x"50";
		f2hValid <= '1';
		wait until falling_edge(f2hReady);
		f2hData <= (others => 'X');
		f2hValid <= '0';

		wait until rising_edge(f2hReady);
		f2hData <= x"AA";
		f2hValid <= '1';
		wait until falling_edge(f2hReady);
		f2hData <= (others => 'X');
		f2hValid <= '0';

		wait until rising_edge(f2hReady);
		f2hData <= x"A0";
		f2hValid <= '1';
		wait until falling_edge(f2hReady);
		f2hData <= (others => 'X');
		f2hValid <= '0';
		wait;
	end process;
	
	-- Drive the sync serial side
	process
	begin
		h2fReady <= '0';
		wait for 110 us;
		h2fReady <= '1';
		wait;
	end process;
end architecture;
