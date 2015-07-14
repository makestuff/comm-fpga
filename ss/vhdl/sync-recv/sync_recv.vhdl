--
-- Copyright (C) 2013 Chris McClelland
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

entity sync_recv is
	port(
		clk_in        : in    std_logic;
		
		-- Serial in
		serClkFE_in   : in    std_logic;  -- serClk falling edge
		serData_in    : in    std_logic;

		-- Parallel out
		recvData_out  : out   std_logic_vector(7 downto 0);
		recvValid_out : out   std_logic
	);
end entity;

architecture rtl of sync_recv is
	type StateType is (
		S_IDLE,
		S_RECV_BITS
	);
	signal state          : StateType := S_IDLE;
	signal state_next     : StateType;
	signal recvCount      : unsigned(2 downto 0) := (others => '0');
	signal recvCount_next : unsigned(2 downto 0);
	signal recvData       : std_logic_vector(6 downto 0) := (others => '0');
	signal recvData_next  : std_logic_vector(6 downto 0);
begin
	-- Infer registers
	process(clk_in)
	begin
		if ( rising_edge(clk_in) ) then
			state       <= state_next;
			recvCount   <= recvCount_next;
			recvData    <= recvData_next;
		end if;
	end process;

	-- Next state logic
	process(
		state, serClkFE_in, serData_in, recvCount, recvData)
	begin
		state_next <= state;
		recvCount_next <= recvCount;
		recvData_next <= recvData;
		recvData_out <= (others => 'X');
		recvValid_out <= '0';
		case state is
			-- Receiving bits
			when S_RECV_BITS =>
				if ( serClkFE_in = '1' ) then
					recvData_next <= serData_in & recvData(6 downto 1);
					recvCount_next <= recvCount - 1;
					if ( recvCount = 0 ) then
						recvData_out <= serData_in & recvData;
						recvValid_out <= '1';
						state_next <= S_IDLE;
					end if;
				end if;

			-- S_IDLE and others
			when others =>
				if ( serClkFE_in = '1' ) then
					if ( serData_in = '0' ) then
						-- This is a start bit; next bit will be data bit zero
						recvCount_next <= "111";
						state_next <= S_RECV_BITS;
					end if;
				end if;
		end case;
	end process;
end architecture;
