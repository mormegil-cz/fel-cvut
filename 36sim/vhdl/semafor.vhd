package SemaforTypy is
   type TDetektorVystup is (Nejede, Jede);
end SemaforTypy;

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use work.SemaforTypy.all;

entity Semafor is
  port (
	NS_auto, EW_auto: in TDetektorVystup;
	CLK: in STD_LOGIC;
	RESET: in STD_LOGIC;
	NS_red, NS_yellow, NS_green,
	EW_red, EW_yellow, EW_green: out STD_LOGIC
  );
end Semafor;

architecture Semafor_Body of Semafor is
type
  TStav is (RR, TR, GR1, GR2, GR3, YR, RT, RG1, RG2, RG3, RY);
type
  TTabulkaPrechodu is array(TStav, TDetektorVystup, TDetektorVystup) of TStav;
type
  TVystup is record
		NSr, NSy, NSg, EWr, EWy, EWg: STD_LOGIC;
  end record;
type
  TTabulkaVystupu is array(TStav) of TVystup;
constant
  TabulkaPrechodu: TTabulkaPrechodu := (
	((RR, RT), (TR, TR)),		--RR
	((GR1, GR1), (GR1, GR1)),	--TR
	((GR2, GR2), (GR2, GR2)),	--GR1
	((GR3, GR3), (GR3, GR3)),	--GR2
	((YR, YR), (YR, YR)),		--GR3
	((RR, RT), (RR, RT)),		--YR
	((RG1, RG1), (RG1, RG1)),	--RT
	((RG2, RG2), (RG2, RG2)),	--RG1
	((RG3, RG3), (RG3, RG3)),	--RG2
	((RY, RY), (RY, RY)),		--RG3
	((RR, RR), (TR, TR))		--RY
  );
constant
  TabulkaVystupu: TTabulkaVystupu := (
	('1', '0', '0', '1', '0', '0'),	--RR
	('1', '1', '0', '1', '0', '0'),	--TR
	('0', '0', '1', '1', '0', '0'),	--GR1
	('0', '0', '1', '1', '0', '0'),	--GR2
	('0', '0', '1', '1', '0', '0'),	--GR3
	('0', '1', '0', '1', '0', '0'),	--YR
	('1', '0', '0', '1', '1', '0'),	--RT
	('1', '0', '0', '0', '0', '1'),	--RG1
	('1', '0', '0', '0', '0', '1'),	--RG2
	('1', '0', '0', '0', '0', '1'),	--RG3
	('1', '0', '0', '0', '1', '0')	--RY
  );
signal
  CurrStav, NextStav: TStav;
begin
  NextStav <= TabulkaPrechodu(CurrStav, NS_auto, EW_auto);
  NS_red <= TabulkaVystupu(CurrStav).NSr;
  NS_yellow <= TabulkaVystupu(CurrStav).NSy;
  NS_green <= TabulkaVystupu(CurrStav).NSg;
  EW_red <= TabulkaVystupu(CurrStav).EWr;
  EW_yellow <= TabulkaVystupu(CurrStav).EWy;
  EW_green <= TabulkaVystupu(CurrStav).EWg;
  process(CLK)
  begin
     if Clk = '1' then
	if Reset = '1' then
	   CurrStav <= RR;
	else
	   CurrStav <= NextStav;
	end if;
     end if;
  end process;
end Semafor_Body;

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use work.SemaforTypy.all;

entity SemaforTestBench is
end SemaforTestBench;

architecture STBBody of SemaforTestBench is
component Semafor
  port (
	NS_auto, EW_auto: in TDetektorVystup;
	CLK: in STD_LOGIC;
	RESET: in STD_LOGIC;
	NS_red, NS_yellow, NS_green,
	EW_red, EW_yellow, EW_green: out STD_LOGIC
  );
end component;
signal S_NSa, S_EWa: TDetektorVystup;
signal S_CLK, S_RESET, S_NSr, S_NSy, S_NSg, S_EWr, S_EWy, S_EWg: STD_LOGIC;
begin
  Sem: Semafor port map (S_NSa, S_EWa, S_CLK, S_RESET, S_NSr, S_NSy, S_NSg, S_EWr, S_EWy, S_EWg);
  Testuj: process
  begin
	-- Nejprve se zresetuje, pak projede jedno auto e-w. Pote soucasne prijedou dve auta z kolmych smeru. Pote, co n-s je propusteno, projede i e-w.

	S_CLK <= '0';
	S_RESET <= '1';

	wait for 10 ns;
	S_CLK <= '1';
	wait for 10 ns;

	S_CLK <= '0';
	S_RESET <= '0';
	S_EWa <= Jede;

	wait for 10 ns;
	S_CLK <= '1';
	wait for 10 ns;

	S_EWa <= Nejede;
	S_CLK <= '0';
	wait for 10 ns;
	S_CLK <= '1';
	wait for 10 ns;
	S_CLK <= '0';
	wait for 10 ns;
	S_CLK <= '1';
	wait for 10 ns;
	S_CLK <= '0';
	wait for 10 ns;
	S_CLK <= '1';
	wait for 10 ns;
	S_CLK <= '0';
	wait for 10 ns;
	S_CLK <= '1';
	wait for 10 ns;
	S_CLK <= '0';
	wait for 10 ns;
	S_CLK <= '1';
	wait for 10 ns;
	S_CLK <= '0';
	wait for 10 ns;
	S_CLK <= '1';
	wait for 10 ns;
	S_CLK <= '0';
	wait for 10 ns;
	S_CLK <= '1';
	wait for 10 ns;

	S_CLK <= '0';
	S_EWa <= Jede;
	S_NSa <= Jede;

	wait for 10 ns;
	S_CLK <= '1';
	wait for 10 ns;

	S_NSa <= Nejede;
	S_CLK <= '0';
	wait for 10 ns;
	S_CLK <= '1';
	wait for 10 ns;
	S_CLK <= '0';
	wait for 10 ns;
	S_CLK <= '1';
	wait for 10 ns;
	S_CLK <= '0';
	wait for 10 ns;
	S_CLK <= '1';
	wait for 10 ns;
	S_CLK <= '0';
	wait for 10 ns;
	S_CLK <= '1';
	wait for 10 ns;
	S_CLK <= '0';
	wait for 10 ns;
	S_CLK <= '1';
	wait for 10 ns;
	S_CLK <= '0';
	wait for 10 ns;
	S_CLK <= '1';
	wait for 10 ns;
	S_CLK <= '0';
	wait for 10 ns;
	S_CLK <= '1';
	wait for 10 ns;

	S_EWa <= Nejede;
	S_CLK <= '0';
	wait for 10 ns;
	S_CLK <= '1';
	wait for 10 ns;
	S_CLK <= '0';
	wait for 10 ns;
	S_CLK <= '1';
	wait for 10 ns;
	S_CLK <= '0';
	wait for 10 ns;
	S_CLK <= '1';
	wait for 10 ns;
	S_CLK <= '0';
	wait for 10 ns;
	S_CLK <= '1';
	wait for 10 ns;
	S_CLK <= '0';
	wait for 10 ns;
	S_CLK <= '1';
	wait for 10 ns;
	S_CLK <= '0';
	wait for 10 ns;
	S_CLK <= '1';
	wait for 10 ns;
	S_CLK <= '0';
	wait for 10 ns;
	S_CLK <= '1';
	wait for 10 ns;


  end process;
end STBBody;


