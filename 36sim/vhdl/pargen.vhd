------------------------------------
--    8-bit EVEN PARITY generator --
------------------------------------

entity ParGen is		-- 8-bit even parity generator
	port(
		A:   in  bit_vector(7 downto 0);	-- Data input (8-bit)
		Clk: in  bit;				-- Clock (parity is recomputed on falling edge)
		P:   out bit				-- Even parity of data
	);
end ParGen;

architecture ParGen of ParGen is
begin
	process(Clk)
	begin
	   if Clk = '0' then
	   	P <= A(0) xor A(1) xor A(2) xor A(3) xor A(4) xor A(5) xor A(6) xor A(7);
	   end if;
	end process;
end ParGen;

