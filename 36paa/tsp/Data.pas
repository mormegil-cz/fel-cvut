unit Data;

interface

const
  NO_PATH = 100;//1E6;

procedure LoadTXT(FN: String);
procedure LoadTSP(FN: String);

implementation
uses SysUtils, Genetics;

procedure LoadTXT(FN: String);
var S: String;
    F: TextFile;
    I, Line: Integer;
    D: TValue;
begin
  AssignFile(F, FN);
  FileMode := 0;
  Reset(F);
  try
     Readln(F, S);
     ChromozomeLength := -1;
     Line := 0;
     while not EOF(F) do
     begin
          Readln(F, S);
          if ChromozomeLength < 0 then
          begin
               ChromozomeLength := Length(S);
               SetLength(DistanceMatrix, ChromozomeLength, ChromozomeLength);
          end;
          if Line >= ChromozomeLength then Break;
          if Length(S) <> ChromozomeLength then raise EInOutError.Create('Bad data file (1)');

          for I := 0 to ChromozomeLength-1 do
          begin
               case S[I+1] of
                    '0': D := NO_PATH;
                    '1'..'9': D := Byte(S[I+1]) - Byte('0');
                    'A'..'Z': D := Byte(S[I+1]) - Byte('A') + 10;
                    else begin raise EInOutError.Create('Bad data file (2)'); D:=0; end;
               end;
               DistanceMatrix[I][Line] := D;
               DistanceMatrix[Line][I] := D;
          end;
          Inc(Line);
     end;
  finally
     CloseFile(F);
  end;
end;

procedure LoadTSP(FN: String);
type
  TPoint = record
                 pX, pY: Integer;
           end;
var F: TextFile;
    Dim: Integer;
    Data: array of TPoint;
    Idx, X, Y, P: Integer;
    S: String;
begin
     Assign(F, FN);
     FileMode := 0;
     Reset(F);
     Dim := -1;
     while not EOF(F) do
     begin
          Readln(F, S);
          if Copy(S, 1, 12) = 'DIMENSION : ' then
          begin
               Dim := StrToInt(Copy(S, 13, Length(S) - 12));
               SetLength(Data, Dim);
               Continue;
          end else if (S = '') or (S[1] < '0') or (S[1] > '9') then Continue;
          Assert(Data <> nil);
          P := Pos(' ', S);
          Assert(P <> 0);
          Idx := StrToInt(Copy(S, 1, P-1));
          Delete(S, 1, P);
          P := Pos(' ', S);
          Assert(P <> 0);
          X := StrToInt(Copy(S, 1, P-1));
          Delete(S, 1, P);
          Y := StrToInt(S);
          Assert((Idx >= 1) and (Idx <= Dim));
          with Data[Idx-1] do
          begin
               pX := X;
               pY := Y;
          end;
     end;
     CloseFile(F);

     Assert(Dim > 0);
     ChromozomeLength := Dim;
     SetLength(DistanceMatrix, ChromozomeLength, ChromozomeLength);

     for X := 0 to ChromozomeLength-1 do
      for Y := 0 to ChromozomeLength-1 do
       if X = Y then DistanceMatrix[X][Y] := NO_PATH
                else DistanceMatrix[X][Y] := Sqrt(Sqr(Data[X].pX - Data[Y].pX) + Sqr(Data[X].pY - Data[Y].pY));
end;

end.

