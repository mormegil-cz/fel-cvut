{$A+,B-,D-,E-,F-,G+,I-,L+,N+,O-,P-,Q-,R-,S-,T-,V+,X+}

unit EmulHW;
{ Core source is in EMULHW.ASM }

interface

procedure InstallHWEmulator;
procedure DeinstallHWEmulator;

procedure StopTracing;

implementation

uses DOS;

const STEP_INT   = $01;
      KBD_INT    = $09;
      BREAK_INT  = $03;

{$F+}

var   Screen: Array[0..63999] of Byte;

procedure SetRGBBlock(First:Byte;Length: Word; var Block); assembler;
asm
   MOV DX, 3C8h
   MOV AL, First
   OUT DX, AL
   MOV CX, Length
   MOV BX, CX
   SHL BX, 1
   ADD CX, BX
   INC DX
   CLD
   PUSH DS
   LDS SI, Block
   REP OUTSB
   POP DS
end;

const Paleta: Array[0..23, 0..2] of Byte = (
              ( 0,  0,  0),
              (20,  0,  0),       { Pedestrian red }
              ( 0, 16,  0),       { Green }
              (16, 16,  0),       { Yellow }
              (20,  0,  0),       { Red }
              ( 0,  0,  0),
              ( 0,  0,  0),
              (45, 45, 45),       { Frames }
              (25, 25, 25),       { Road }
              (63,  0,  0),       { Pedestrian red }
              ( 0, 63,  0),       { Green - lit }
              (63, 63,  0),       { Yellow - lit }
              (63,  0,  0),       { Red - lit }
              ( 0,  0,  0),
              ( 0,  0,  0),
              (63, 63, 63),       { Road markings }
              (63, 32,  0),       { Pedestrian red - lit }
              ( 0,  0,  0),
              (63, 63,  0),       { Green - middle highlight }
              (63, 63,  0),       { Yellow - middle higlight }
              (63, 32,  0),       { Red - middle highlight }
              ( 0,  0,  0),
              ( 0,  0,  0),
              ( 0,  0,  0)
      );

procedure StepISR; external;
procedure BreakISR; external;
procedure EmulHWData; external;
{$L EMULHW.OBJ}

type TEmulHWData = packed record
                          RestoreCode: Byte;
                          RestoreCS,
                          RestoreIP: Word;
                          RestoreBreak: Byte;
                          BreakSet: Byte;
                          DoExitProg: Byte;
                          StopTracing: Byte;
                          CurrState: Byte;
                          SwitchState,
                          ButtonState: Byte;
                          VehicleState: Word;
                          PedestrState,
                          LEDState: Byte;
                          eUnselectedInput,
                          eOutputToInput,
                          eInputFromOutput: Longint;
                          {DebugData: Word;
                          BuffPos: Word;
                          Buffer: Array[0..127] of Word;}
                   end;
     PEmulHWData = ^TEmulHWData;
     PPointer    = ^Pointer;

const
      Installed: Boolean = False;

      OldTrap:   Pointer = Nil;
      OldExit:   Pointer = Nil;
      OldKeyboardInt: Procedure = nil;
      OldBreak:  Pointer = Nil;

      pData:     PEmulHWData = @EmulHWData;

procedure DrawButton(X, Y, Color: Byte);
begin
     Mem[$A000:(320*Y)+X]:=Color;
     Mem[$A000:(320*(Y+1))+X-1]:=Color;
     Mem[$A000:(320*(Y+1))+X]:=Color;
     Mem[$A000:(320*(Y+1))+X+1]:=Color;
     Mem[$A000:(320*(Y+2))+X]:=Color;
end;

procedure DrawSwitch(X, Y: Byte; On: Boolean);
var dX, dY: Shortint;
begin
     for dY:=-4 to +4 do Mem[$A000:320*(Y+dY)+X]:=0;

     for dY:=-1 to +1 do
      for dX:=-1 to +1 do Mem[$A000:320*(Y+dY)+X+dX]:=7;

     if On then for dY:=0 to +4 do Mem[$A000:320*(Y+dY)+X]:=15
           else for dY:=-4 to 0 do Mem[$A000:320*(Y+dY)+X]:=15;
end;

procedure KeyboardISR; interrupt;
begin
     asm
        IN   AL, 61h
        XCHG AL, AH
        OR   AL, 80h
        OUT  61h, AL
        XCHG AL, AH
        OUT  61h, AL

        MOV  AL, 20h
        OUT  20h, AL
     end;
     case Port[$60] of
             1: if (Mem[0:$417] and 3<>0) and (Mem[0:$417] and $C=$C) then
                   asm                        { Ctrl+Alt+Shift+ESC - terminate emulated program }
                      MOV AX, 4C00h
                      INT 21h
                   end
                   else pData^.DoExitProg:=1; { ESC - abort emulated program }
            72: begin { Up - north button pressed }
                     pData^.ButtonState:=pData^.ButtonState and $EF;
                     DrawButton(76, 50, 15);
                end;
           200: begin { Up released - north button released }
                     pData^.ButtonState:=pData^.ButtonState or $10;
                     DrawButton(76, 50,  0);
                end;
            75: begin { Left - west button pressed }
                     pData^.ButtonState:=pData^.ButtonState and $BF;
                     DrawButton(50, 123, 15);
                end;
           203: begin { Left released - west button released }
                     pData^.ButtonState:=pData^.ButtonState or $40;
                     DrawButton(50, 123,  0);
                end;
            77: begin { Right - east button pressed }
                     pData^.ButtonState:=pData^.ButtonState and $DF;
                     DrawButton(150, 75, 15);
                end;
           205: begin { Right released - east button released }
                     pData^.ButtonState:=pData^.ButtonState or $20;
                     DrawButton(150, 75,  0);
                end;
            80: begin { Down - south button pressed }
                     pData^.ButtonState:=pData^.ButtonState and $F7;
                     DrawButton(124, 150, 15);
                end;
           208: begin { Down released - south button released }
                     pData^.ButtonState:=pData^.ButtonState or $08;
                     DrawButton(124, 150,  0);
                end;
           59:  begin { F1 pressed - switch 1 switched }
                     pData^.SwitchState:=pData^.SwitchState xor $40;
                     DrawSwitch(160, 170, pData^.SwitchState and $40=0);
                end;
           60:  begin { F2 pressed - switch 2 switched }
                     pData^.SwitchState:=pData^.SwitchState xor $20;
                     DrawSwitch(170, 170, pData^.SwitchState and $20=0);
                end;
           61:  begin { F3 pressed - switch 3 switched }
                     pData^.SwitchState:=pData^.SwitchState xor $10;
                     DrawSwitch(180, 170, pData^.SwitchState and $10=0);
                end;
           62:  begin { F4 pressed - switch 4 switched }
                     pData^.SwitchState:=pData^.SwitchState xor $08;
                     DrawSwitch(190, 170, pData^.SwitchState and $08=0);
                end;
           63:  begin { F5 pressed - refresh screen }
                     asm
                        MOV  AX, 13h
                        INT  10h
                     end;
                     SetRGBBlock(0,24,Paleta);
                     Move(Screen, Ptr($A000,0)^, SizeOf(Screen));
                end;
           else begin
                     asm
                        PUSHF
                     end;
                     OldKeyboardInt;
                end;
     end;
end;
{$F-}

procedure InstallHWEmulator;
var F: File;
begin
     if Installed then Exit;

     FileMode:=0;
     Assign(F, 'CROSS.RAW');
     Reset(F, 1);
     if IOResult<>0 then
     begin
          Writeln('Chyba: Nebyl nalezen soubor CROSS.RAW!');
          Halt(1);
     end;

     OldExit:=ExitProc;
     ExitProc:=@DeinstallHWEmulator;

     GetIntVec(KBD_INT, @OldKeyboardInt);
     GetIntVec(STEP_INT, OldTrap);
     GetIntVec(BREAK_INT, OldBreak);

     Installed:=True;

     { TODO : Store current videomode and later restore, instead of hard-set mode 3 }
     asm
        MOV AX, 13h
        INT 10h
     end;
     SetRGBBlock(0,24,Paleta);

     BlockRead(F, Screen, SizeOf(Screen));
     Move(Screen, Ptr($A000,0)^, SizeOf(Screen));
     Close(F);

     SetIntVec(KBD_INT, @KeyboardISR);
     SetIntVec(STEP_INT, @StepISR);
     SetIntVec(BREAK_INT, @BreakISR);
end;

function Hex(w: Word): String;
const
 hexChars: array [0..$F] of Char =
   '0123456789ABCDEF';
begin
 Hex:=hexChars[Hi(w) shr 4]+
      hexChars[Hi(w) and $F]+
      hexChars[Lo(w) shr 4]+
      hexChars[Lo(w) and $F];
end;

procedure DeinstallHWEmulator;
var I: INTEGER;
begin
     if ExitProc=@DeinstallHWEmulator then ExitProc:=OldExit;

     if not Installed then Exit;

     SetIntVec(BREAK_INT, OldBreak);
     SetIntVec(STEP_INT, OldTrap);
     SetIntVec(KBD_INT, @OldKeyboardInt);

     asm
        MOV  AX, 3
        INT  10h
     end;

     with pData^ do
     begin
          if (eUnselectedInput<>0) or (eOutputToInput<>0) or
             (eInputFromOutput<>0) then Writeln('Byly detekovany tyto chyby:');
          if eUnselectedInput<>0 then Writeln(eUnselectedInput,'x cteni bez nastaveni multiplexu');
          if eOutputToInput<>0 then Writeln(eOutputToInput,'x zapis na vstupni port');
          if eInputFromOutput<>0 then Writeln(eInputFromOutput,'x cteni z vystupniho portu');

{          Writeln('DoExitProg=',DoExitProg);
          Writeln('VehicleState=',Hex(VehicleState));
          Writeln('CurrState=',Hex(CurrState));
          Writeln('PedestrState=',Hex(PedestrState));
          Writeln('LEDState=',Hex(LEDState));
          Writeln('RestoreCode=',Hex(RestoreCode));
          restorecs,
          restoreip,
          restorebreak
          Writeln('CurrState=',Hex(CurrState));
          Writeln('SwitchState=',Hex(SwitchState));
          Writeln('ButtonState=',Hex(ButtonState));

          Writeln('Debug data: ',DebugData);
          Writeln('Buffer from position ',BuffPos,':');
          for I:=128 downto 1 do Write(Hex(Buffer[((BuffPos+256-2*I) mod 256) div 2]),' ');}
     end;
end;

procedure StopTracing;
begin
     pData^.StopTracing:=1;
end;

end.