uses CRT;

const
      { COM port }
      cpCOM1=0;
      cpCOM2=1;
      cpCOM3=2;
      cpCOM4=3;

      { Transfer speed (=115200/BAUD) }
      cp1200BAUD  =96;
      cp2400BAUD  =48;
      cp4800BAUD  =24;
      cp9600BAUD  =12;
      cp19200BAUD =6;
      cp38400BAUD =3;
      cp57600BAUD =2;
      cp115200BAUD=1;

      { Stop bits }
      cp1Stop=$00;
      cp2Stop=$04;

      { Data bits }
      cp5Bits=$00;
      cp6Bits=$01;
      cp7Bits=$02;
      cp8Bits=$03;

      { Parity }
      cpNoParity   =$00;
      cpOddParity  =$08;
      cpEvenParity =$18;
      cpMarkParity =$28;
      cpSpaceParity=$38;

      { Send break }
      cpBreakSignal=$40;

type TCOMError=(ceOK,         { No error }
                ceInvArg,     { Invalid argument }
                ceBuffFull,   { Output buffer full }
                ceBuffEmpty,  { Input buffer empty }
                ceTimeout);   { Timeout occured while waiting for ready status }

var COM: Byte;
    C:   Char;

    COM_Port: Word;
    COM_Status: TCOMError;

    X1, Y1, X2, Y2: Integer;

begin
   if ParamCount = 0 then COM := cpCOM1
   else if ParamStr(1)='/1' then COM := cpCOM1
   else if ParamStr(1)='/2' then COM := cpCOM2
   else begin
             Writeln('Usage: CHAT [ /1 | /2 ]');
             Halt(2);
        end;

   Write('Initializing communication...');

   X1 := 1; Y1 := 1;
   X2 := 1; Y2 := 1;

   asm
   MOV  BL, COM
   XOR  BH, BH
   SHL  BX, 1
   ADD  BX, 400h
   PUSH 0
   POP  ES
   MOV  DX, ES:[BX]
   MOV  COM_Port,DX

   TEST DX, DX
   JNZ  @OK_COM

   MOV  COM_Status, ceInvArg
   JMP  @Exit
  @OK_COM:
   INC  DX
   MOV  AL, 0F0h
   OUT  DX, AL          { Reset }

   ADD  DX, 2
   MOV  AL, 80h
   OUT  DX, AL          { Set DLAB for setting BAUD rate }

   MOV  AX, cp9600BAUD
   SUB  DX, 3
   OUT  DX, AL
   XCHG AH, AL
   INC  DX
   OUT  DX, AL          { Set BAUD rate }

   ADD  DX, 2
   MOV  AL, cp1Stop or cp8Bits or cpNoParity
   OUT  DX, AL          { Set comm. params - bits,parity,... }

   SUB  DX, 2
   XOR  AL, AL
   OUT  DX, AL          { disable IRQ }

   ADD  DX, 3
   MOV  AL, 09h
   OUT  DX, AL          { LoopBack=off, GPO2=on, GPO1=off, DTR=on, RTS=off }

   MOV  COM_Status, ceOK
  @Exit:
   end;

   if COM_Status <> ceOk then
   begin
        case COM_Status of
             ceInvArg: Writeln('The requested COM port is not available.');
             else      Writeln('Unable to init.');
        end;
        Halt(1);
   end;

   ClrScr;
   GotoXY(1, 13);
   Write('컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴');
   GotoXY(1, 1);

   repeat
         repeat
         until KeyPressed or (Port[COM_Port+5] and 1 = 1);

         if Port[COM_Port+5] and 1 = 1 then
         begin
              C := Char(Port[COM_Port]);
              Window(1, 14, 80, 25);
              GotoXY(X2, Y2);

              case C of
                   #13: begin
                             X2 := 1;
                             Inc(Y2);
                             if Y2 = 13 then
                             begin
                                  Dec(Y2);
                                  GotoXY(1, 1);
                                  DelLine;
                             end;
                        end;
                   #10: ;
                   else begin
                             Write(C);
                             Inc(X2);
                             if X2 = 81 then
                             begin
                                  X2 := 1;
                                  if Y2 < 12 then Inc(Y2);
                             end;
                        end;
              end;
         end;

         Window(1, 1, 80, 12);
         GotoXY(X1, Y1);

         if KeyPressed then
         begin
              C := ReadKey;

              case C of
                   #27: ;
                   #13: begin
                             X1 := 1;
                             Inc(Y1);
                             if Y1 = 13 then
                             begin
                                  Dec(Y1);
                                  GotoXY(1, 1);
                                  DelLine;
                             end;
                        end;
                   else begin
                             Write(C);
                             Inc(X1);
                             if X1 = 81 then
                             begin
                                  X1 := 1;
                                  if Y1 < 12 then Inc(Y1);
                             end;
                        end;
              end;

              if C <> #27 then Port[COM_Port] := Byte(C);
         end;
         GotoXY(X1, Y1);
   until C = #27;

   Window(1,1,80,25);
   ClrScr;
   Writeln('Bye!');
end.