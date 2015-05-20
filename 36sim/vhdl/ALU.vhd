----------------
-- Simple ALU --
----------------

package ALUTypes is
	type tALUOpcode is (ADD, SUB, MUL, DIV);	-- ALU operation code
end ALUTypes;

use work.ALUTypes.all;
entity ALU is
	port(
		A,B: in  Integer;		-- operands A,B
		Clk: in  Bit;			-- clock (falling edge-trigerred)
		Op:  in  tALUOpcode;		-- operation code
		C:   out Integer		-- operation result
	);
end ALU;

architecture ALU of ALU is
begin
   process(Clk)
   begin
	if Clk = '0' then	-- clock falling edge only
	   case Op is
		when ADD => C <= A + B;
		when SUB => C <= A - B;
		when MUL => C <= A * B;
		when DIV => C <= A / B;
	   end case;
	end if;
   end process;
end ALU;

------------------------------- Tester --------------------------------

use work.ALUTypes.all;
entity Tester is
end Tester;

--use work.ALUTypes.all;
architecture Tester of Tester is
signal aA,aB: Integer;
signal aClk: Bit;
signal aOp:  tALUOpcode;
signal aC: Integer;
component ALU
	port(
		A,B: in  Integer;
		Clk: in  Bit;
		Op:  in  tALUOpcode;
		C:   out Integer
	);
end component;
begin
  S: ALU port map (aA,aB,aClk,aOp,aC);
  G: process
     begin
	aA<=123;
	aB<=345;
	aOp<=ADD;
	aClk<='1' after 10 ns;
	aClk<='0' after 20 ns;
	wait for 100 ns;
	aA<=345;
	aB<=123;
	aOp<=SUB;
	aClk<='1' after 10 ns;
	aClk<='0' after 20 ns;
	wait for 100 ns;
	aA<=11;
	aB<=12;
	aOp<=MUL;
	aClk<='1' after 10 ns;
	aClk<='0' after 20 ns;
	wait for 100 ns;
	aA<=156;
	aB<=12;
	aOp<=DIV;
	aClk<='1' after 10 ns;
	aClk<='0' after 20 ns;
	wait for 100 ns;
	wait for 100 ns;
     end process;
end Tester;



