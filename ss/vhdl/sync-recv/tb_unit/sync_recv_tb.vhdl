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

entity sync_recv_tb is
end entity;

architecture behavioural of sync_recv_tb is
	signal sysClk     : std_logic;  -- main system clock
	signal dispClk    : std_logic;  -- display version of sysClk, which leads it by 4ns

	-- Serial in
	signal serClk     : std_logic;
	signal serData    : std_logic;
	
	-- Parallel out
	signal recvData   : std_logic_vector(7 downto 0);
	signal recvValid  : std_logic;

	-- Detect rising serClk edges
	signal serClk_prev : std_logic;
	signal serClkFE    : std_logic;
begin
	-- Instantiate sync_recv module for testing
	uut: entity work.sync_recv
		port map(
			clk_in        => sysClk,

			-- Serial in
			serClkFE_in   => serClkFE,
			serData_in    => serData,

			-- Parallel out
			recvData_out  => recvData,
			recvValid_out => recvValid
		);

	-- Infer registers
	process(sysClk)
	begin
		if ( rising_edge(sysClk) ) then
			serClk_prev <= serClk;
		end if;
	end process;

	-- Detect rising edges on serClk
	serClkFE <=
		'1' when serClk = '0' and serClk_prev = '1'
		else '0';

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

	-- Drive the sync serial signals
	process
		procedure sendByte(constant b : in std_logic_vector(7 downto 0)) is
		begin
			serData <= '0';   -- start bit
			wait until rising_edge(serClk); serData <= b(0);  -- bit 0
			wait until rising_edge(serClk); serData <= b(1);  -- bit 1
			wait until rising_edge(serClk); serData <= b(2);  -- bit 2
			wait until rising_edge(serClk); serData <= b(3);  -- bit 3
			wait until rising_edge(serClk); serData <= b(4);  -- bit 4
			wait until rising_edge(serClk); serData <= b(5);  -- bit 5
			wait until rising_edge(serClk); serData <= b(6);  -- bit 6
			wait until rising_edge(serClk); serData <= b(7);  -- bit 7
			wait until rising_edge(serClk); serData <= '1';   -- stop bit
			wait until rising_edge(serClk);
		end procedure;
		procedure pause(constant n : in integer) is
			variable i : integer;
		begin
			for i in 1 to n loop
				wait until rising_edge(serClk);
			end loop;
		end procedure;
	begin
		serData <= '1';
		pause(4);
		sendByte(x"55");
		sendByte(x"5B");
		sendByte(x"5A");
		serData <= 'Z';  -- tri-state data line after final send (AVR disables sender)
		wait;
	end process;

end architecture;
