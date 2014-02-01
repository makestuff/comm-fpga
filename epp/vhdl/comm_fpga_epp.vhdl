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

entity comm_fpga_epp is
	port(
		clk_in         : in    std_logic;                     -- clock input (asynchronous with EPP signals)
		reset_in       : in    std_logic;                     -- synchronous active-high reset input
		reset_out      : out   std_logic;                     -- synchronous active-high reset output

		-- EPP interface -----------------------------------------------------------------------------
		eppData_io    : inout std_logic_vector(7 downto 0);   -- bidirectional 8-bit data bus
		eppAddrStb_in : in    std_logic;                      -- active-low asynchronous address strobe
		eppDataStb_in : in    std_logic;                      -- active-low asynchronous data strobe
		eppWrite_in   : in    std_logic;                      -- read='1'; write='0'
		eppWait_out   : out   std_logic;                      -- active-low asynchronous wait signal

		-- Channel read/write interface --------------------------------------------------------------
		chanAddr_out   : out   std_logic_vector(6 downto 0);  -- the selected channel (0-127)

		-- Host >> FPGA pipe:
		h2fData_out    : out   std_logic_vector(7 downto 0);  -- data lines used when the host writes to a channel
		h2fValid_out   : out   std_logic;                     -- '1' means "on the next clock rising edge, please accept the data on h2fData_out"
		h2fReady_in    : in    std_logic;                     -- channel logic can drive this low to say "I'm not ready for more data yet"

		-- Host << FPGA pipe:
		f2hData_in     : in    std_logic_vector(7 downto 0);  -- data lines used when the host reads from a channel
		f2hValid_in    : in    std_logic;                     -- channel logic can drive this low to say "I don't have data ready for you"
		f2hReady_out   : out   std_logic                      -- '1' means "on the next clock rising edge, put your next byte of data on f2hData_in"
	);
end entity;

architecture rtl of comm_fpga_epp is
	type StateType is (
		S_RESET,
		S_IDLE,
		S_ADDR_WRITE_WAIT,
		S_DATA_WRITE_EXEC,
		S_DATA_WRITE_WAIT,
		S_DATA_READ_EXEC,
		S_DATA_READ_WAIT
	);

	-- State and next-state
	signal state           : StateType := S_RESET;
	signal state_next      : StateType;
	
	-- Synchronised versions of asynchronous inputs
	signal eppAddrStb_sync : std_logic := '1';
	signal eppDataStb_sync : std_logic := '1';
	signal eppWrite_sync   : std_logic := '1';
	
	-- Registers
	signal eppWait         : std_logic := '1';
	signal eppWait_next    : std_logic;
	signal chanAddr        : std_logic_vector(6 downto 0) := (others => '0');
	signal chanAddr_next   : std_logic_vector(6 downto 0);
	signal eppData         : std_logic_vector(7 downto 0) := (others => '0');
	signal eppData_next    : std_logic_vector(7 downto 0);

	-- Other signals
	signal driveBus        : std_logic := '0';  -- whether or not to drive eppData_io
begin
	-- Infer registers
	process(clk_in)
	begin
		if ( rising_edge(clk_in) ) then
			if ( reset_in = '1' ) then
				state           <= S_RESET;
				chanAddr        <= (others => '0');
				eppData         <= (others => '0');
				eppWait         <= '1';
				eppAddrStb_sync <= '1';
				eppDataStb_sync <= '1';
				eppWrite_sync   <= '1';
			else
				state           <= state_next;
				chanAddr        <= chanAddr_next;
				eppData         <= eppData_next;
				eppWait         <= eppWait_next;
				eppAddrStb_sync <= eppAddrStb_in;
				eppDataStb_sync <= eppDataStb_in;
				eppWrite_sync   <= eppWrite_in;
			end if;
		end if;
	end process;

	-- Next state logic
	process(
		state, eppData_io, eppAddrStb_sync, eppDataStb_sync, eppWrite_sync, chanAddr, eppWait, eppData,
		f2hData_in, f2hValid_in, h2fReady_in)
	begin
		state_next <= state;
		chanAddr_next <= chanAddr;
		eppWait_next <= eppWait;
		eppData_next <= eppData;
		h2fData_out <= (others => '0');
		f2hReady_out <= '0';
		h2fValid_out <= '0';
		reset_out <= '0';
		driveBus <= eppWrite_sync;
		case state is
			-- Finish the address update cycle
			when S_ADDR_WRITE_WAIT =>
				if ( eppAddrStb_sync = '1' ) then
					eppWait_next <= '0';
					state_next <= S_IDLE;
				end if;

			-- Host writes a byte to the FPGA
			when S_DATA_WRITE_EXEC =>
				h2fData_out <= eppData_io;
				h2fValid_out <= '1';
				if ( h2fReady_in = '1') then
					eppWait_next <= '1';
					state_next <= S_DATA_WRITE_WAIT;
				end if;
			when S_DATA_WRITE_WAIT =>
				if ( eppDataStb_sync = '1' ) then
					eppWait_next <= '0';
					state_next <= S_IDLE;
				end if;

			-- Host reads a byte from the FPGA
			when S_DATA_READ_EXEC =>
				eppData_next <= f2hData_in;
				f2hReady_out <= '1';
				if ( f2hValid_in = '1' ) then
					eppWait_next <= '1';
					state_next <= S_DATA_READ_WAIT;
				end if;
			when S_DATA_READ_WAIT =>
				if ( eppDataStb_sync = '1' ) then
					eppWait_next <= '0';
					state_next <= S_IDLE;
				end if;
				
			-- S_RESET - tri-state everything
			when S_RESET =>
				reset_out <= '1';
				driveBus <= '0';
				if ( eppWrite_sync = '0' ) then
					state_next <= S_IDLE;
				end if;
				
			-- S_IDLE and others
			when others =>
				eppWait_next <= '0';
				if ( eppAddrStb_sync = '0' ) then
					-- Address can only be written, not read
					if ( eppWrite_sync = '0' ) then
						eppWait_next <= '1';
						chanAddr_next <= eppData_io(6 downto 0);
						state_next <= S_ADDR_WRITE_WAIT;
					end if;
				elsif ( eppDataStb_sync = '0' ) then
					-- Register read or write
					if ( eppWrite_sync = '0' ) then
						state_next <= S_DATA_WRITE_EXEC;
					else
						state_next <= S_DATA_READ_EXEC;
					end if;
				end if;
		end case;
	end process;

	-- Drive stateless signals
	chanAddr_out <= chanAddr;
	eppWait_out <= eppWait;
	eppData_io <=
		eppData when ( driveBus = '1' ) else
		"ZZZZZZZZ";
end architecture;
