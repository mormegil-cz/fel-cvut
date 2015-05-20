uses CRT;

const
      IRQ_Base = 8;

      { COM port }
      cpCOM1=0;
      cpCOM2=1;
      cpCOM3=2;
      cpCOM4=3;

      { COM IRQ }
      cpCOM1IRQ=4;
      cpCOM2IRQ=3;
      cpCOM3IRQ=4;
      cpCOM4IRQ=3;

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
    IRQ: Byte;
    C:   Char;

    COM_Port: Word;
    COM_Status: TCOMError;

    X1, Y1, X2, Y2: Integer;

    OldISR: Procedure;
    OldPICMask: Byte;
    ISR_Lock: Boolean;
    X: Byte;

procedure ISR_Routine; interrupt;
var Why: Byte;
    C:   Char;
begin
     if ISR_Lock then
     begin
          { Nastalo reentrantni vyvolani! -> ERROR! }
          Port[$20]:=$20;
          Exit;
     end;
     ISR_Lock:=True;

     Why:=Port[COM_Port+2];
     if (Why and 1)=1 then
     begin
          { UART nezpracovava zadne preruseni! -> ERROR!! }
          { Zavolal me nekdo cizi! }
          Port[$20]:=$20;
          ISR_Lock:=False;
          Exit;
     end;

     {if Why and 6 = 2 then}
     begin
          C := Char(Port[COM_Port]);
          Window(1, 14, 80, 25);
          GotoXY(X2, Y2);

          case C of
               #10: ;
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
               else begin
                         Write(C);
                         Inc(X2);
                         if X2 = 81 then
                         begin
                              Inc(Y2);
                              if Y2 = 13 then
                              begin
                                   Dec(Y2);
                                   GotoXY(1, 1);
                                   DelLine;
                              end;
                         end;
                    end;
          end;
         Window(1, 1, 80, 12);
         GotoXY(X1, Y1);
     end;

     Port[$20]:=$20;
     ISR_Lock:=False;
end;

begin
   if ParamCount = 0 then COM := cpCOM1
   else if ParamStr(1)='/1' then COM := cpCOM1
   else if ParamStr(1)='/2' then COM := cpCOM2
   else begin
             Writeln('Usage: CHAT [ /1 | /2 ]');
             Halt(2);
        end;

   case COM of
        cpCOM1: IRQ := IRQ_Base + cpCOM1IRQ;
        cpCOM2: IRQ := IRQ_Base + cpCOM2IRQ;
        cpCOM3: IRQ := IRQ_Base + cpCOM3IRQ;
        cpCOM4: IRQ := IRQ_Base + cpCOM4IRQ;
        else Halt(3);
   end;

   Write('Initializing communication...');

   X1 := 1; Y1 := 1;
   X2 := 1; Y2 := 1;

   asm
   MOV  ISR_Lock, TRUE

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
   MOV  AL, 1
   OUT  DX, AL          { IRQ on Character Receive }

   ADD  DX, 3
   MOV  AL, 09h
   OUT  DX, AL          { LoopBack=off, GPO2=on, GPO1=off, DTR=on, RTS=off }

   MOV  AH, 35h         { GetIntVec(IRQ,@OldISR) }
   MOV  AL, IRQ
   INT  21h
   MOV  WORD PTR [OldISR], BX
   MOV  WORD PTR [OldISR+2], ES

   PUSH DS              { SetIntVec(IRQ,@ISR_Routine) }
   MOV  AH, 25h
   PUSH CS
   POP  DS
   MOV  DX, OFFSET ISR_Routine
   INT  21h
   POP  DS

   MOV  DX, 21h
   IN   AL, DX
   MOV  OldPICMask, AL
   MOV  CL, IRQ
   SUB  CL, IRQ_Base
   MOV  BL, 1
   SHL  BL, CL
   OR   BL, 04h
   NOT  BL
   AND  AL, BL
   OUT  DX, AL          { Enable COMx on PIC }

   MOV  ISR_Lock, FALSE
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
         Window(1, 1, 80, 12);
         GotoXY(X1, Y1);

         C := ReadKey;

         Window(1, 1, 80, 12);
         GotoXY(X1, Y1);

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
   until C = #27;

   Window(1,1,80,25);
   ClrScr;
   asm
   MOV  ISR_Lock, TRUE
   MOV  DX, WORD PTR COM_Port
   ADD  DX, 4

   SUB  DX, 2
   OUT  DX, AL             { Disable IRQ on character receive }

   MOV  DX, 21h
   MOV  AL, OldPICMask
   OUT  DX, AL             { Restore PIC settings }

   PUSH DS
   MOV  AH, 25h
   MOV  AL, IRQ
   MOV  BX, WORD PTR [OldISR+2]
   MOV  DX, WORD PTR [OldISR]
   MOV  DS, BX
   INT  21h
   POP  DS                 { SetIntVec(WhichINT,@OldISR); }
   MOV  COM_Status, ceOK
   end;
   Writeln('Bye!');
end.