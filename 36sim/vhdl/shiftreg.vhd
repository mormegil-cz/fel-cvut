library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity ShiftRegister is
   port (
	SerialIn: in STD_LOGIC;
	ParallelOut: buffer STD_LOGIC_VECTOR;
	SerialOut: buffer STD_LOGIC;
	Clk: in STD_LOGIC
   );
end ShiftRegister;

architecture ShiftRegister_Body of ShiftRegister is
begin
   process(Clk)
   begin
	if Clk = '0' then
	   SerialOut <= ParallelOut(ParallelOut'High);
	   for I in ParallelOut'High downto ParallelOut'Low+1 loop
		ParallelOut(I) <= ParallelOut(I-1);
	   end loop;
	   ParallelOut(ParallelOut'Low) <= SerialIn;
	end if;
   end process;
end ShiftRegister_Body;

entity ShiftRegTester is
end ShiftRegTester;

library IEEE;
use IEEE.STD_LOGIC_1164.all;

architecture ShiftRegTester of ShiftRegTester is
signal rSerialIn: STD_LOGIC;
signal rParallelOut: STD_LOGIC_VECTOR(7 downto 0);
signal rSerialOut: STD_LOGIC;
signal rClk: STD_LOGIC;
component ShiftRegister
   port (
	SerialIn: in STD_LOGIC;
	ParallelOut: buffer STD_LOGIC_VECTOR;
	SerialOut: buffer STD_LOGIC;
	Clk: in STD_LOGIC
   );
end component;
begin
  Reg: ShiftRegister port map (rSerialIn, rParallelOut, rSerialOut, rClk);
  Testuj: process
	begin
		rSerialIn <= '0';
		wait for 10 ns;
		rSerialIn <= '1';
		wait for 10 ns;
		rSerialIn <= '0';
		wait for 10 ns;
		rSerialIn <= '0';
		wait for 10 ns;
		rSerialIn <= '1';
		wait for 30 ns;
	end process;
end ShiftRegTester;

