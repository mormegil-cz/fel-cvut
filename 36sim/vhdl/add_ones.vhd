-----------------------------------------------
-- 1-bit Adder

library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity Adder1 is
  port (
	A, B, C: in STD_LOGIC;
	P, Q: out STD_LOGIC
  );
end Adder1;

architecture Adder1_Body of Adder1 is
begin
   P <= A xor B xor C;
   Q <= (A and B) or (A and C) or (B and C);
end Adder1_Body;

-----------------------------------------------
-- N-bit adder

library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity AdderN is
  generic (N: Positive);
  port (
	A, B: in STD_LOGIC_VECTOR(N-1 downto 0);
	C: in STD_LOGIC;
	P: out STD_LOGIC_VECTOR(N downto 0)
  );
end AdderN;

architecture AdderN_Body of AdderN is
component Adder1
  port (
	A, B, C: in STD_LOGIC;
	P, Q: out STD_LOGIC
  );
end component;
signal Carry: STD_LOGIC_VECTOR(N downto 0);
begin
   Carry(0) <= C;
   Adders: 
	for I in 0 to N-1 generate
	   AdderX: Adder1 port map (A(I), B(I), Carry(I), P(I), Carry(I+1));
	end generate;
   P(N) <= Carry(N);
end AdderN_Body;

-----------------------------------------------
-- One counter

library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity OneCounter is
  port (
	A: in STD_LOGIC_VECTOR(31 downto 0);
	Y: out STD_LOGIC_VECTOR(5 downto 0)
  );
end OneCounter;

architecture OneCounter_Body of OneCounter is
component Adder1
  port (
	A, B, C: in STD_LOGIC;
	P, Q: out STD_LOGIC
  );
end component;
component AdderN
  generic (N: Positive);
  port (
	A, B: in STD_LOGIC_VECTOR(N-1 downto 0);
	C: in STD_LOGIC;
	P: out STD_LOGIC_VECTOR(N downto 0)
  );
end component;
signal Level1: STD_LOGIC_VECTOR(15 downto 0);
signal Level2: STD_LOGIC_VECTOR(11 downto 0);
signal Level3: STD_LOGIC_VECTOR(7 downto 0);
signal Level4: STD_LOGIC_VECTOR(4 downto 0);
signal FiveZeroes: STD_LOGIC_VECTOR(4 downto 0);
begin
  FiveZeroes <= "00000";
  Adders1:
	for I in 0 to 7 generate
	   Adder1_X: Adder1 port map (A(3*I), A(3*I+1), A(3*I+2), Level1(2*I), Level1(2*I+1));
	end generate;
  Adders2:
	for I in 0 to 3 generate
	   Adder2_X: AdderN generic map(2) port map (Level1(4*I+1 downto 4*I), Level1(4*I+3 downto 4*I+2), A(24+I), Level2(3*I+2 downto 3*I));
	end generate;
  Adders3:
	for I in 0 to 1 generate
	   Adder3_X: AdderN generic map(3) port map (Level2(6*I+2 downto 6*I), Level2(6*I+5 downto 6*I+3), A(28+I), Level3(4*I+3 downto 4*I));
	end generate;
  Adder4: AdderN generic map(4) port map (Level3(3 downto 0), Level3(7 downto 4), A(30), Level4);
  Adder5: AdderN generic map(5) port map (FiveZeroes, Level4, A(31), Y);
end OneCounter_Body;

-----------------------------------------------
-- Test bench

library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity AddOnes_Tester is
end AddOnes_Tester;

architecture AddOnes_Tester_Body of AddOnes_Tester is
component OneCounter
  port (
	A: in STD_LOGIC_VECTOR(31 downto 0);
	Y: out STD_LOGIC_VECTOR(5 downto 0)
  );
end component;
signal TestVstup: STD_LOGIC_VECTOR(31 downto 0);
signal TestVystup: STD_LOGIC_VECTOR(5 downto 0);
begin
  Soucastka: OneCounter port map (TestVstup, TestVystup);
  Testuj: process
	begin
		TestVstup <= "00000000000000000000000000000000";	-- 0 ones  [000000]
		wait for 10 ns;
		TestVstup <= "00000000000000000000000000000001";	-- 1 one   [000001]
		wait for 10 ns;
		TestVstup <= "10000000000000000000000000000000";	-- 1 one   [000001]
		wait for 10 ns;
		TestVstup <= "10000011100000001110000001000000";        -- 8 ones  [001000]
		wait for 10 ns;
		TestVstup <= "01110000111000100110000110011111";	-- 16 ones [010000]
		wait for 10 ns;
		TestVstup <= "11111101111111101111110110111110";	-- 27 ones [011011]
		wait for 10 ns;
		TestVstup <= "11111111111111111111111111111111";	-- 32 ones [100000]
		wait for 10 ns;
	end process;
end AddOnes_Tester_Body;

