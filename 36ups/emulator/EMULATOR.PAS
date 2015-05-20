{$A+,B-,D-,E-,F-,G+,I-,L+,N+,O-,P-,Q-,R-,S-,T-,V-,X+}
{$M 16384,0,0}
program Emulator;

uses EXE_Load, EmulHW;

{ UNIMPLEMENTED FEATURES:

                * program must be EXE, COM is not supported
}

procedure RepairFName(var FName: String); { Add .EXE if extension missing }
var I: Byte;
begin
     I:=Length(FName);
     while (I>0) and (FName[I]<>'\') and (FName[I]<>'.') do Dec(I);
     if (I>0) and (FName[I]='\') then FName:=FName+'.EXE';
end;

var EXEName: String;
exitc:word;

begin
     Writeln('Emulator pripravku SEMAFOR pro predmet UPS na FEL CVUT');
     Writeln('verze 1.1');
     Writeln('Copyright (C) DragonSoft 2000');
     Writeln;
     if ParamCount<>1 then
     begin
          Writeln('Pouziti: EMULATOR program[.EXE]');
          Writeln('         kde program je jmeno vaseho programu pro rizeni krizovatky');
          Halt(1);
     end;
     EXEName:=ParamStr(1);
     RepairFName(EXEName);

     if not LoadEXE(EXEName) then
     begin
          Writeln('Chyba nacitani programu: ',ErrorMsg);
          Halt(1);
     end;

     InstallHWEmulator;
     ExecuteProgram;
     StopTracing;
     asm
        { Clear the TF flag if not already cleared - well, it SHOULD BE! }
        PUSHF
        POP  AX
        AND  AX, 0FEFFh
        PUSH AX
        POPF
     end;
     DeinstallHWEmulator;

     {DumpEXE;}

     Writeln('Emulator pripravku SEMAFOR pro predmet UPS na FEL CVUT');
     Writeln('verze 1.1');
     Writeln('Copyright (C) DragonSoft 2000');
     Writeln;
     Writeln('Emulator ukoncen');
     Writeln('Nashledanou!');
end.