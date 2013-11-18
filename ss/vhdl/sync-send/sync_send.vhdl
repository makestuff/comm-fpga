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

entity sync_send is
	port(
		clk_in        : in  std_logic;
		
		-- Serial I/O
		serClkRE_in   : in  std_logic;
		serData_out   : out std_logic;

		-- Parallel in
		sendData_in   : in  std_logic_vector(7 downto 0);
		sendValid_in  : in  std_logic;
		sendReady_out : out std_logic
	);
end entity;

architecture rtl of sync_send is
	type StateType is (
		S_IDLE,
		S_WAIT,
		S_SEND_BITS
	);
	signal state          : StateType := S_IDLE;
	signal state_next     : StateType;
	signal sendCount      : unsigned(3 downto 0) := (others => '0');
	signal sendCount_next : unsigned(3 downto 0);
	signal sendData       : std_logic_vector(8 downto 0) := (others => '0');
	signal sendData_next  : std_logic_vector(8 downto 0);
begin
	-- Infer registers
	process(clk_in)
	begin
		if ( rising_edge(clk_in) ) then
			state       <= state_next;
			sendCount   <= sendCount_next;
			sendData    <= sendData_next;
		end if;
	end process;

	-- Next state logic
	process(
		state, serClkRE_in, sendData_in, sendValid_in, sendCount, sendData)
	begin
		state_next <= state;
		sendCount_next <= sendCount;
		sendData_next <= sendData;
		sendReady_out <= '0';
		serData_out <= '1';
		case state is
			-- Sending bits
			when S_SEND_BITS =>
				serData_out <= sendData(0);
				if ( serClkRE_in = '1' ) then
					sendData_next <= "1" & sendData(8 downto 1);
					sendCount_next <= sendCount - 1;
					if ( sendCount = 1 ) then
						state_next <= S_IDLE;
					end if;
				end if;

			-- Got a byte to send, waiting for rising_edge(serClk)
			when S_WAIT =>
				if ( serClkRE_in = '1' ) then
					state_next <= S_SEND_BITS;
				end if;
				
			-- S_IDLE and others
			when others =>
				sendReady_out <= '1';
				if ( sendValid_in = '1' ) then
					-- There's a byte ready to be sent
					sendCount_next <= x"9";
					sendData_next <= sendData_in & "0";
					if ( serClkRE_in = '1' ) then
						state_next <= S_SEND_BITS;
					else
						state_next <= S_WAIT;
					end if;
				end if;
		end case;
	end process;

end architecture;
