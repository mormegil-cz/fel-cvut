{$G+}
uses Crt;

procedure SetGDCReg(Reg, Value: Byte); assembler;
asm
   MOV  AL, Reg
   MOV  AH, Value
   MOV  DX, 3CEh
   OUT  DX, AX
end;

procedure SetSEQReg(Reg, Value: Byte); assembler;
asm
   MOV  AL, Reg
   MOV  AH, Value
   MOV  DX, 3C4h
   OUT  DX, AX
end;

const
  OldMode: Byte = 3;

procedure InitGraphicsMode; assembler;
asm
   MOV  AH, 0Fh
   INT  10h
   MOV  OldMode, AL
   MOV  AX, 0012h
   INT  10h
end;

procedure CloseGraphicsMode; assembler;
asm
   XOR  AH, AH
   MOV  AL, OldMode
   INT  10h
end;

procedure DrawHorizLine(X1, Y, X2: Integer; Color: Byte);
var Ofs, Ofs2: Word;
begin
     if X1 > X2 then
     begin
          { swap X1, X2 }
          X1 := X1 xor X2;
          X2 := X1 xor X2;
          X1 := X1 xor X2;
     end;
     if X2 < 0 then Exit;
     if (Y < 0) or (Y > 479) then Exit;
     if X1 < 0 then X1 := 0;
     if X2 > 639 then X2 := 639;

     SetGDCReg(0, Color);       { Set/Reset - color to be stored }
     SetGDCReg(1, $F);          { Enable Set/Reset - all planes use reg 0 as color }
     SetGDCReg(3, 0);           { Data rotate - replace, 0 rotations }
     SetGDCReg(5, 0);           { Graphic mode - write mode 0 }

     asm
        MOV  AX, 0A000h
        MOV  ES, AX
     end;
     if (X1 and $FFF8) = (X2 and $FFF8) then
     begin
          { the whole line is in a single byte }
          Ofs := X1 shr 3 + Y*80;
          SetGDCReg(8, (1 shl (X1 and 7 + 1) - 1) and not (1 shl (X2 and 7) - 1));
     end else
     begin
          { left 0-7 pixels }
          Ofs := X1 shr 3 + Y*80;
          Ofs2 := X2 shr 3 + Y*80;
          if X1 and 7 <> 0 then
          begin
               SetGDCReg(8, 1 shl (X1 and 7 + 1) - 1);
               asm
                  MOV  DI, Ofs
                  OR   BYTE PTR ES:[DI], AL        { AL ignored }
               end;
          end;
          { middle pixels }
          if Ofs+1 <> Ofs2 then
          begin
               SetGDCReg(8, $FF);
               asm
                  MOV  DI, Ofs
                  MOV  CX, Ofs2
                  SUB  CX, DI
                  INC  DI
                 @NextPixel:
                  OR   ES:[DI], AL             { AL ignored }
                  INC  DI
                  LOOP @NextPixel
               end;
          end;
          { right 0-7 pixels }
          if X2 and 7 <> 0 then
          begin
               SetGDCReg(8, not (1 shl (X2 and 7) - 1));
               asm
                  MOV  DI, Ofs2
                  OR   ES:[DI], AL        { AL ignored }
               end;
          end;
     end;
end;

procedure DrawVertLine(X, Y1, Y2: Integer; Color: Byte);
var Ofs: Word;
begin
     if Y1 > Y2 then
     begin
          { swap Y1, Y2 }
          Y1 := Y1 xor Y2;
          Y2 := Y1 xor Y2;
          Y1 := Y1 xor Y2;
     end;
     if Y2 < 0 then Exit;
     if (X < 0) or (X > 639) then Exit;
     if Y1 < 0 then Y1 := 0;
     if Y2 > 479 then Y2 := 479;

     SetGDCReg(0, Color);       { Set/Reset - color to be stored }
     SetGDCReg(1, $F);          { Enable Set/Reset - all planes use reg 0 as color }
     SetGDCReg(3, 0);           { Data rotate - replace, 0 rotations }
     SetGDCReg(5, 0);           { Graphic mode - write mode 0 }
     SetGDCReg(8, 1 shl (X and 7)); { Bit mask - the correct pixel from the 8 }
     Ofs := X shr 3 + Y1*80;
     asm
        MOV  DI, Ofs
        MOV  AX, 0A000h
        MOV  ES, AX
        MOV  CX, Y2
        SUB  CX, Y1
        INC  CX
       @NextLine:
        OR   ES:[DI], AL        { AL ignored }
        ADD  DI, 80
        LOOP @NextLine
     end;
end;

procedure FillRect(X1, Y1, X2, Y2: Integer; Color: Byte);
var Y: Integer;
begin
     if Y1 > Y2 then
     begin
          { swap Y1, Y2 }
          Y1 := Y1 xor Y2;
          Y2 := Y1 xor Y2;
          Y1 := Y1 xor Y2;
     end;
     if Y2 < 0 then Exit;
     if Y1 < 0 then Y1 := 0;
     if Y2 > 479 then Y2 := 479;
     if X1 > X2 then
     begin
          { swap X1, X2 }
          X1 := X1 xor X2;
          X2 := X1 xor X2;
          X1 := X1 xor X2;
     end;
     if X2 < 0 then Exit;
     if X1 < 0 then X1 := 0;
     if X2 > 639 then X2 := 639;

     for Y:=Y1 to Y2 do
         DrawHorizLine(X1, Y, X2, Color);
end;

procedure PutPixel(X, Y: Integer; Color: Byte);
{var Ofs: Word;}
begin
     if (X < 0) or (X > 639) or
        (Y < 0) or (Y > 479) then Exit;

     SetGDCReg(0, Color);       { Set/Reset - color to be stored }
     SetGDCReg(1, $F);          { Enable Set/Reset - all planes use reg 0 as color }
     SetGDCReg(3, 0);           { Data rotate - replace, 0 rotations }
     SetGDCReg(5, 0);           { Graphic mode - write mode 0 }
     SetGDCReg(8, 1 shl (X and 7)); { Bit mask - the correct pixel from the 8 }
     {Ofs := X shr 3 + Y*80;}
     asm
        {MOV  DI, Ofs}

        MOV  DI, X
        SHR  DI, 3
        MOV  AX, Y
        MOV  BX, AX
        SHL  AX, 4
        SHL  BX, 6
        ADD  DI, AX
        ADD  DI, BX

        MOV  AX, 0A000h
        MOV  ES, AX
       @NextLine:
        OR   ES:[DI], AL        { AL ignored }
     end;
end;

type TBitmap = array[0..7] of Byte;
procedure PutBitmap8(X, Y: Integer; const Bitmap: TBitmap);
var I: Byte;
begin
     for I := 0 to 7 do
         PutPixel(X+I, Y, Bitmap[I]);
end;

procedure SetAttributeMap(Color, DACColor: Byte); assembler;
asm
   MOV  DX, 3DAh
   IN   AL, DX
   MOV  DX, 3C0h
   MOV  AL, Color
   OUT  DX, AL
   JMP  @Wait
  @Wait:
   MOV  AL, DACColor
   OUT  DX, AL
   JMP  @Wait2
  @Wait2:
   MOV  AL, 20h
   OUT  DX, AL
end;

procedure SetColorPal(Color: Byte; R, G, B: Byte); assembler;
asm
   MOV  AL, Color
   MOV  DX, 3C8h
   OUT  DX, AL
   INC  DX
   MOV  AL, R
   OUT  DX, AL
   MOV  AL, G
   OUT  DX, AL
   MOV  AL, B
   OUT  DX, AL
end;

{ Saved screen file format:

 offset    size     contents
-----------------------------------------------------
      0   768 B     DAC Palette (R0,G0,B0,R1,G1,B1,...)
    768 38400 B     Plane 0 data
  39168 38400 B     Plane 1 data
  77568 38400 B     Plane 2 data
 115968 38400 B     Plane 3 data

 154368             End of file
-----------------------------------------------------}

procedure SaveScreen(const Filename: string);
var F: File;
    Pal: Array[0..767] of Byte;
    Plane: Byte;
begin
     { read palette }
     asm
        PUSH SS
        POP  ES
        LEA  DI, Pal
        MOV  CX, 768
        MOV  DX, 3C7h
        XOR  AL, AL
        OUT  DX, AL
        INC  DX
        INC  DX
        CLD
        REP  INSB
     end;

     FileMode := 1;
     Assign(F, FileName);
     Rewrite(F, 1);
     BlockWrite(F, Pal, SizeOf(Pal));
     { bitplane data }
     SetGDCReg(5, 0);           { Graphic mode - read mode 0 }
     for Plane:=0 to 3 do
     begin
          SetGDCReg(4, Plane);  { Read map select - choose the plane }
          BlockWrite(F, Ptr($A000, $0000)^, 38400);
     end;
     Close(F);
end;

procedure LoadScreen(const Filename: string);
var F: File;
    Pal: Array[0..767] of Byte;
    Plane: Byte;
    Data: Pointer;
begin
     FileMode := 0;
     Assign(F, FileName);
     Reset(F, 1);
     BlockRead(F, Pal, SizeOf(Pal));
     { set palette }
     asm
        PUSH DS
        PUSH SS
        POP  DS
        LEA  SI, Pal
        MOV  CX, 768
        MOV  DX, 3C8h
        XOR  AL, AL
        OUT  DX, AL
        INC  DX
        CLD
        REP  OUTSB
        POP  DS
     end;

     { bitplane data }
     SetGDCReg(1, 0);   { Enable Set/Reset - all planes use CPU byte as color }
     SetGDCReg(3, 0);   { Data rotate - replace, 0 rotations }
     SetGDCReg(5, 0);   { Graphic mode - write mode 0 }
     SetGDCReg(8, $FF); { Bit mask - the whole byte is written }
     GetMem(Data, 38400);
     for Plane:=0 to 3 do
     begin
          SetSEQReg(2, 1 shl Plane); { Map mask - write only into the current plane }
          BlockRead(F, Data^, 38400);
          asm
             PUSH DS
             MOV  AX, 0A000h
             MOV  ES, AX
             XOR  DI, DI
             LDS  SI, Data
             MOV  CX, 38400
             CLD
             REP  MOVSB
             POP  DS
          end;
     end;
     FreeMem(Data, 38400);
     Close(F);
end;

const
  R: array[0..3] of Byte = (32, 32, 32, 32);
  G: array[0..3] of Byte = ( 0,  0, 10, 20);
  B: array[0..3] of Byte = ( 0, 20, 10,  0);

  Bitmap: TBitmap = ( 15, 15, 14, 14, 14, 14, 15, 15 );

var
  BIOS_Timer: Longint absolute $0040:$006C;
  Count,
  Start,
  Stop: Longint;
  Color: Byte;

  Sytost, Jas, Ton: Byte;

begin
     InitGraphicsMode;
     Randomize;

     { Uloha A }
     for Color:=0 to 15 do
         FillRect((Color shr 2) * 120, (Color and 3) * 120, (Color shr 2+1)*120, (Color and 3+1) * 120, Color);

     ReadKey;

     DrawHorizLine(0, 100, 639, 0);
     DrawVertLine(100, 0, 479, 0);

     ReadKey;

     { Uloha B }

     for Ton := 0 to 15 do
         SetAttributeMap(Ton, Ton);

     for Jas := 0 to 15 do
         SetColorPal(Jas, 4*Jas, 0, 0);

     {for Ton := 0 to 3 do
       for Jas := 0 to 1 do
         for Sytost := 0 to 1 do
              SetColorPal(Jas shl 3 + Sytost shl 2 + Ton,
                          R[Ton] + 31*Jas + Sytost * (32-R[Ton]) shr 1,
                          G[Ton] + 31*Jas + Sytost * (32-G[Ton]) shr 1,
                          B[Ton] + 31*Jas + Sytost * (32-B[Ton]) shr 1
                         );}

     { Uloha C }
     {PutBitmap8(31, 20, Bitmap);
     PutBitmap8(62, 21, Bitmap);
     PutBitmap8(93, 22, Bitmap);
     PutBitmap8(124, 23, Bitmap);
     PutBitmap8(155, 24, Bitmap);}

     {Test putpix
     for Ton := 0 to 100 do
       for Jas := 0 to 100 do
         PutPixel(Ton, Jas, 14);}

     ReadKey;

     {Count := 0;
     Start := BIOS_Timer;
     while not KeyPressed do
     begin
          DrawVertLine(Random(640), Random(480), Random(480), Random(16));
          DrawHorizLine(Random(640), Random(480), Random(640), Random(16));
          Inc(Count);
     end;
     Stop := BIOS_Timer;
     ReadKey;}

     SaveScreen('TEST.SHT');
     ReadKey;

     asm
        MOV  AX, 12h
        INT  10h
     end;
     ReadKey;

     for Ton := 0 to 15 do
         SetAttributeMap(Ton, Ton);

     LoadScreen('TEST.SHT');
     ReadKey;

     CloseGraphicsMode;
     {Writeln(Count, 'x2 lines, ', (Stop-Start)*0.055:0:3, 'sec, ',18.2*Count/(Stop-Start):0:0, 'x2 lines/sec');}
end.
