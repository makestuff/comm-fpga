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

entity comm_fpga_ss is
	port(
		clk_in       : in  std_logic;
		
		-- Serial interface --------------------------------------------------------------------------
		serClk_in    : in  std_logic;
		serData_in   : in  std_logic;
		serData_out  : out std_logic;
		
		-- Channel read/write interface --------------------------------------------------------------
		chanAddr_out : out std_logic_vector(6 downto 0);
		
		-- Host >> FPGA pipe:
		h2fData_out  : out std_logic_vector(7 downto 0);
		h2fValid_out : out std_logic;
		h2fReady_in  : in  std_logic;
		
		-- Host << FPGA pipe:
		f2hData_in   : in  std_logic_vector(7 downto 0);
		f2hValid_in  : in  std_logic;
		f2hReady_out : out std_logic
	);
end entity;

architecture rtl of comm_fpga_ss is
	type StateType is (
		S_IDLE,        -- wait for requst from host & regster isRead & chanAddr
		S_GET_COUNT0,  -- wait for count high byte
		S_GET_COUNT1,  -- wait for count high mid byte
		S_GET_COUNT2,  -- wait for count low mid byte
		S_GET_COUNT3,  -- wait for count low byte
		S_WRITE,       -- writing data to FPGA application
		S_WAIT_STOP,   -- wait for the stop bit in preparation for a read
		S_WAIT_READ,   -- wait for microcontroller to assert serData_sync, indicating readiness
		S_READ,        -- send data to microcontroller
		S_END_READ     -- wait for micro to deassert serData_sync, indicating acknowledgement
	);
	signal state          : StateType := S_IDLE;
	signal state_next     : StateType;
	signal count          : unsigned(31 downto 0) := (others => '0');
	signal count_next     : unsigned(31 downto 0);
	signal isRead         : std_logic := '0';
	signal isRead_next    : std_logic;
	signal chanAddr       : std_logic_vector(6 downto 0) := (others => '0');
	signal chanAddr_next  : std_logic_vector(6 downto 0);
	signal serData_sync   : std_logic;
	signal serClk_sync    : std_logic;
	signal serClk_prev    : std_logic;
	signal serDataIn      : std_logic;
	signal serDataOut     : std_logic;
	signal serClkRE       : std_logic;
	signal serClkFE       : std_logic;
	signal recvData       : std_logic_vector(7 downto 0);
	signal recvValid      : std_logic;
	signal sendValid      : std_logic;
	signal sendReady      : std_logic;
	signal fifoInputData  : std_logic_vector(7 downto 0);
	signal fifoInputValid : std_logic;
	signal fifoDepth      : std_logic_vector(2 downto 0);
	signal writeThrottle  : std_logic;
begin
	-- Infer registers
	process(clk_in)
	begin
		if ( rising_edge(clk_in) ) then
			state        <= state_next;
			count        <= count_next;
			isRead       <= isRead_next;
			chanAddr     <= chanAddr_next;
			serClk_sync  <= serClk_in;
			serClk_prev  <= serClk_sync;
			serData_sync <= serData_in;
		end if;
	end process;
	
	-- Next state logic
	process(
		state, count, recvData, recvValid, sendReady, f2hValid_in, serDataOut, serData_sync,
		fifoDepth, isRead, chanAddr, writeThrottle)
	begin
		state_next     <= state;
		count_next     <= count;
		isRead_next    <= isRead;
		chanAddr_next  <= chanAddr;
		fifoInputData  <= (others => 'X');
		fifoInputValid <= '0';
		serData_out    <= '1';  -- default not ready
		serDataIn      <= serData_sync;
		f2hReady_out   <= '0';
		sendValid      <= '0';
		case state is
			-- Get the count high byte
			when S_GET_COUNT0 =>
				serData_out <= '0';  -- ready
				if ( recvValid = '1' ) then
					count_next(31 downto 24) <= unsigned(recvData);
					state_next <= S_GET_COUNT1;
				end if;
			
			-- Get the count high mid byte
			when S_GET_COUNT1 =>
				serData_out <= '0';  -- ready
				if ( recvValid = '1' ) then
					count_next(23 downto 16) <= unsigned(recvData);
					state_next <= S_GET_COUNT2;
				end if;
			
			-- Get the count low mid byte
			when S_GET_COUNT2 =>
				serData_out <= '0';  -- ready
				if ( recvValid = '1' ) then
					count_next(15 downto 8) <= unsigned(recvData);
					state_next <= S_GET_COUNT3;
				end if;
			
			-- Get the count low byte
			when S_GET_COUNT3 =>
				serData_out <= '0';  -- ready
				if ( recvValid = '1' ) then
					count_next(7 downto 0) <= unsigned(recvData);
					if ( isRead = '1' ) then
						state_next <= S_WAIT_STOP;
					else
						state_next <= S_WRITE;
					end if;
				end if;
			
			-- Host is writing
			when S_WRITE =>
				serData_out <= writeThrottle;
				if ( recvValid = '1' ) then
					-- We got a data byte from the host
					fifoInputData <= recvData;
					fifoInputValid <= '1';
					count_next <= count - 1;
					if ( count = 1 ) then
						state_next <= S_IDLE;
					end if;
				end if;
			
			-- Wait for the stop bit in preparation for a read
			when S_WAIT_STOP =>
				serDataIn <= '1';    -- isolate sync-recv unit from serData_in
				serData_out <= '1';  -- we'll start writing soon
				if ( serData_sync = '1' ) then
					-- Other side has started its stop bit, so proceed
					state_next <= S_WAIT_READ;
				end if;
			
			-- Wait for microcontroller to assert serData_sync, indicating readiness
			when S_WAIT_READ =>
				serDataIn <= '1';    -- isolate sync-recv unit from serData_in
				serData_out <= '1';  -- we'll start writing soon
				if ( serData_sync = '0' ) then
					-- Other side is ready to receive, so proceed
					state_next <= S_READ;
				end if;
			
			-- Send "count" bytes of data, throttling if necessary
			when S_READ =>
				serDataIn <= '1';    -- isolate sync-recv unit from serData_in
				serData_out <= serDataOut;
				if ( serData_sync = '0' ) then
					sendValid <= f2hValid_in;
					f2hReady_out <= sendReady;
					if ( f2hValid_in = '1' and sendReady = '1' ) then
						count_next <= count - 1;
						if ( count = 1 ) then
							state_next <= S_END_READ;
						end if;
					end if;
				end if;
			
			-- Wait for microcontroller to deassert serData_sync, indicating acknowledgement
			when S_END_READ =>
				serDataIn <= '1';    -- isolate sync-recv unit from serData_in
				serData_out <= serDataOut;
				if ( serData_sync = '1' ) then
					-- Other side has finished receiving, so proceed
					state_next <= S_IDLE;
				end if;
			
			-- S_IDLE and others
			when others =>
				if ( fifoDepth = "000" ) then
					serData_out <= '0';  -- OK maybe we're ready after all
					if ( recvValid = '1' ) then
						-- We got a byte from the host - it's a message length
						isRead_next <= recvData(7);
						chanAddr_next <= recvData(6 downto 0);
						state_next <= S_GET_COUNT0;
					end if;
				end if;
		end case;
	end process;
	
	-- Clock edges, rising and falling:
	serClkRE <=
		'1' when serClk_sync = '1' and serClk_prev = '0'
		else '0';
	serClkFE <=
		'1' when serClk_sync = '0' and serClk_prev = '1'
		else '0';
	
	-- Drive out channel
	chanAddr_out <= chanAddr;
	
	-- Throttle writes
	writeThrottle <=
		'1' when to_integer(unsigned(fifoDepth)) > 1
		else '0';
	
	-- Synchronous serial send unit
	sync_send: entity work.sync_send
		port map(
			clk_in        => clk_in,
			
			-- Serial out
			serClkRE_in   => serClkRE,
			serData_out   => serDataOut,
			
			-- Parallel in
			sendData_in   => f2hData_in,
			sendValid_in  => sendValid,
			sendReady_out => sendReady
		);
	
	-- Synchronous serial receive unit
	sync_recv: entity work.sync_recv
		port map(
			clk_in        => clk_in,
			
			-- Serial in
			serClkFE_in   => serClkFE,
			serData_in    => serDataIn,
			
			-- Parallel out
			recvData_out  => recvData,
			recvValid_out => recvValid
		);
	
	-- TODO: Can this small buffer FIFO be eliminated?
	write_fifo: entity work.fifo
		generic map(
			WIDTH => 8,
			DEPTH => 2
		)
		port map(
			clk_in          => clk_in,
			reset_in        => '0',
			depth_out       => fifoDepth,
			
			-- Input pipe
			inputData_in    => fifoInputData,
			inputValid_in   => fifoInputValid,
			inputReady_out  => open,
			
			-- Output pipe
			outputData_out  => h2fData_out,
			outputValid_out => h2fValid_out,
			outputReady_in  => h2fReady_in
		);
end architecture;
