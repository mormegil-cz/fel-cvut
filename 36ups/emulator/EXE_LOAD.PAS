{$A+,B-,D-,E-,F-,G+,I-,L+,N+,O-,P-,Q-,R-,S-,T-,V+,X+}

unit EXE_Load;

interface

function LoadEXE(const FileName: String): Boolean; { True=OK }
procedure ExecuteProgram;

procedure DumpEXE;

var ErrorMsg: String;

implementation
uses DOS, EmulHW;

{$I ERROR.INC}

type TFar = packed record
                  O, S: Word;
            end;

     TPSP = packed record
                   INT20: Word;
                   MemTop: Word;
                   Reserved1: Byte;
                   CALLDispatch: Array[1..5] of Byte;
                   TermAddr: Pointer;
                   CtrlBreak: Pointer;
                   INT24Handler: Pointer;
                   ParentPSP: Word;
                   Reserved2: Array[1..$14] of Byte;
                   Environment: Word;
                   Reserved3: Array[1..$2E] of Byte;
                   FCB1: Array[1..$10] of Byte;
                   FCB2: Array[1..$14] of Byte;
                   Params: String[$7F];
            end;

     TEXEHeader = packed record
                         Magic: Word;
                         PartPage: Word;
                         PageCount: Word;
                         ReloCount: Word;
                         HeaderSize: Word;
                         MinMemory: Word;
                         MaxMemory: Word;
                         ReloSS: Word;
                         ExeSP: Word;
                         ChkSum: Word;
                         ExeIP: Word;
                         ReloCS: Word;
                         TableOfs: Word;
                         Overlay: Word;
                  end;

const MAX_EXE_SIZE = 65000;

      MIN_POOL     = 16;        { Number of bytes guaranteed after the end of the EXE program }

var   Head: TEXEHeader;
      Code,
      EXE:  Pointer;

function LoadEXE(const FileName: String): Boolean;
var F: File;
    Size: Longint;
    Offs: Longint;

    I:    Word;

    I_OFF, I_SEG,
    RELO_SEG, RelocData: Word;

    EXESize: Longint;
    MemSize: Longint;
    MemPara: Word;
begin
     LoadEXE:=False;

     FileMode:=0;
     Assign(F, FileName);
{$I-}
     Reset(F, 1);
{$I+}
     if IOResult<>0 then
     begin
          ErrorMsg:='Soubor nenalezen';
          Exit;
     end;

     { Load EXE header }
     BlockRead(F, Head, SizeOf(Head), I);

     if (Head.Magic<>$5A4D) or (I<>SizeOf(Head)) then
     begin
          ErrorMsg:='Toto neni platny EXE soubor';
          Close(F);
          Exit;
     end;

     EXESize:=FileSize(F);

     Offs:=Head.HeaderSize*16;
     Size:=(Head.PageCount-1)*512+Head.PartPage-Offs;
     if Head.PartPage=0 then Inc(Size, 512);         { DOS bug? - if partpage=0, it means 512 }

     MemSize:=Size+Head.MinMemory*16+MIN_POOL;
     if MemSize>MAX_EXE_SIZE then
     begin
          ErrorMsg:='Program EXE je prilis velky';
          Close(F);
          Exit;
     end;

     MemPara:=(MemSize+15) div 16;
     asm
        MOV  AH, 48h
        MOV  BX, MemPara
        INT  21h
        JNC  @OK
        XOR  AX, AX
       @OK:
        MOV  TFar(Code).S, AX
        XOR  AX, AX
        MOV  TFar(Code).O, AX
     end;
     if Code=nil then
     begin
          ErrorMsg:='Nedostatek pameti pro EXE program';
          Close(F);
          Exit;
     end;

     { Create PSP }
     asm
        MOV  AH, 26h
        MOV  DX, TFar(Code).S
        INT  21h
     end;

     EXE:=Code;
     Inc(TFar(EXE).S, $10);

     Seek(F, Offs);
     BlockRead(F, EXE^, Size);

     Seek(F, Head.TableOfs);
     for I:=1 to Head.ReloCount do
     begin
          BlockRead(F, I_OFF, 2);
          BlockRead(F, I_SEG, 2);
          RELO_SEG:=TFar(EXE).S+I_SEG;
          MemW[RELO_SEG:I_OFF]:=MemW[RELO_SEG:I_OFF]+TFar(EXE).S;
     end;
     Close(F);

     LoadEXE:=True;
end;

procedure ExecuteProgram;
var ToLabel:  Pointer;
label ProgramEnd,
      DataArea;
begin
     asm
        MOV  AX, CS
        MOV  TFar(ToLabel).S, AX
        MOV  TFar(ToLabel).O, OFFSET ProgramEnd
     end;
     TPSP(Code^).TermAddr:=ToLabel;

     asm
        MOV  CS:[OFFSET DataArea], DS
        MOV  CS:[OFFSET DataArea+2], SS
        MOV  CS:[OFFSET DataArea+4], SP
        MOV  CS:[OFFSET DataArea+6], BP

        MOV  AH, 2Fh
        INT  21h
        MOV  CS:[OFFSET DataArea+8], ES
        MOV  CS:[OFFSET DataArea+0Ah], BX

        MOV  AH, 62h
        INT  21h
        MOV  CS:[OFFSET DataArea+0Ch], BX

        JMP  @ContinueCode
DataArea:
        DW   0  { +0    DS }
        DW   0  { +2    SS }
        DW   0  { +4    SP }
        DW   0  { +6    BP }
        DW   0  { +8    DTA seg }
        DW   0  { +A    DTA ofs }
        DW   0  { +C    PSP seg }
@ContinueCode:
        MOV  AH, 50h
        MOV  BX, TFar(Code).S
        INT  21h

        MOV  AX, TFar(EXE).S
        MOV  BX, AX
        ADD  BX, Head.ReloCS
        MOV  CX, Head.EXEIP

        ADD  AX, Head.ReloSS
        MOV  SS, AX
        MOV  SP, Head.EXESP

        MOV  AX, TFar(Code).S
        MOV  ES, AX
        MOV  DS, AX

        XOR  AX, AX
        JMP  @StoreRetAddr
@RunProgram:

        { Set the TF flag and push Flags onto stack }
        PUSHF
        POP  AX
        OR   AX, 0100h
        PUSH AX

        PUSH BX
        PUSH CX
        IRET                   { Jump to EXE and set the TF flag simultaneously }

@StoreRetAddr:
        CALL @RunProgram       { Store return address }
ProgramEnd:
        MOV  AX, CS:[OFFSET DataArea]
        MOV  DS, AX
        MOV  AX, CS:[OFFSET DataArea+2]
        MOV  SS, AX
        MOV  SP, CS:[OFFSET DataArea+4]
        MOV  BP, CS:[OFFSET DataArea+6]

        PUSH DS

        MOV  AH, 1Ah
        MOV  BX, CS:[OFFSET DataArea+8]
        MOV  DS, BX
        MOV  DX, CS:[OFFSET DataArea+0Ah]
        INT  21h

        POP  DS

        MOV  AH, 50h
        MOV  BX, CS:[OFFSET DataArea+0Ch]
        INT  21h

        PUSHF                  { Clear the TF flag }
        POP  AX
        AND  AX, 0FEFFh
        PUSH AX
        POPF
     end;
end;

procedure DumpEXE;
var F: File;
    Offs,Size: Longint;
begin
     Offs:=Head.HeaderSize*16;
     Size:=((Head.PageCount-1)*512)+Head.PartPage-Offs+MIN_POOL;
     Assign(F,'EXEDUMP.RAW');
     Rewrite(F,1);
     BlockWrite(F, EXE^, Size);
     Close(F);
end;

end.