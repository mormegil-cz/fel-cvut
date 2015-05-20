package AutomatTypy is
	type TVstup is (A,B,C);
	type TVystup is (P,Q);
end AutomatTypy;

library IEEE;
use IEEE.STD_LOGIC_1164.all;
use work.AutomatTypy.all;

entity KonecnyAutomat is
   port (
	Vstup: in TVstup;
    	Clock: in STD_LOGIC;
	Vystup: out TVystup
   );
end KonecnyAutomat;

architecture AutomatBody of KonecnyAutomat is
type TStav is (SS, SL, LS, LL);
signal Stav, DalsiStav: TStav;
begin
   Output: 
	process(Stav)
	begin
	   if (Stav = SL) then Vystup <= P after 10 ns;
	      else Vystup <= Q after 10 ns;
	   end if;
	end process;
   NextState:
	process(Stav, Vstup)
	begin
	   case Stav is
		when SS =>
			if (Vstup = A) then DalsiStav <= LS after 10 ns;
			   elsif (Vstup = B) then DalsiStav <= SL after 10 ns;
			end if;
		when LS =>
			if (Vstup = A) then DalsiStav <= SS after 10 ns;
			   elsif (Vstup = B) then DalsiStav <= LL after 10 ns;
			end if;
		when SL =>
			if (Vstup = A) then DalsiStav <= LL after 10 ns;
			   elsif (Vstup = B) then DalsiStav <= SS after 10 ns;
			end if;
		when LL =>
			if (Vstup = A) then DalsiStav <= SL after 10 ns;
			   elsif (Vstup = B) then DalsiStav <= LS after 10 ns;
			end if;
	   end case;
	end process;
   CurrState:
	process(Clock)
	begin
	   if (Clock = '1') then
		Stav <= DalsiStav after 10 ns;
	   end if;
	end process;
end AutomatBody;


