entity Invertor is
	port (
	  Vstup: in BIT;
	  Vystup: out BIT
	);
end Invertor;

architecture Invertor_Body of Invertor is
begin
	Invertuj: process(Vstup)
		  begin
			Vystup <= not(Vstup) after 10 ns;
		  end process;
end Invertor_Body;





