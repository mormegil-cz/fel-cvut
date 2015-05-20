(******************************************************************************
 *                                                                            *
 *    Delphi Component Help Creator                                           *
 *    version 0.1                                                             *
 *                                                                            *
 *    Semestralni prace z predmetu PJ                                         *
 *    Petr Kadlec <kadlecp2@fel.cvut.cz>                                      *
 *                                                                            *
 ******************************************************************************)

{ Input routines for reading program source text } 
unit HCInput;

interface
uses Classes;

type TSourceFile = class
                   private
                     FileName: string;
                     F:        TFileStream;
                     Buffer:   string;
                     BuffPos:  Integer;
                     LineNum:  Cardinal;
                   public
                     constructor Create(const FileName: string);
                     destructor  Destroy; override;

                     function    EOF: Boolean;
                     function    GetChar: Char;
                     procedure   UnGetChar(const C: Char);

                     function    FormatMsg(const Msg: string): string;

                     property    LineNumber: Cardinal read LineNum;
                   end;

const CR = #13;
      LF = #10;

implementation
uses SysUtils;

const BUFFSIZE = 1024;

{ TSourceFile }

constructor TSourceFile.Create(const FileName: string);
begin
  Self.FileName := FileName;
  F := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  LineNum := 1;
end;

destructor TSourceFile.Destroy;
begin
  F.Free;
  inherited;
end;

function TSourceFile.EOF: Boolean;
begin
  Result := (BuffPos = Length(Buffer)) and (F.Position = F.Size);
end;

function TSourceFile.FormatMsg(const Msg: string): string;
begin
     FmtStr(Result, '%s(%d): %s', [FileName, LineNum, Msg]);
end;

function TSourceFile.GetChar: Char;
begin
  Assert(BuffPos<=Length(Buffer));
  if BuffPos=Length(Buffer) then
  begin
       // no character in buffer => read from the stream
       SetLength(Buffer, BUFFSIZE);
       SetLength(Buffer, F.Read(Buffer[1], BUFFSIZE));
       if Buffer='' then
          raise EReadError.Create('Attempt to read after EOF');

       Buffer := AdjustLineBreaks(Buffer);
       BuffPos:=0;
  end;

  Inc(BuffPos);
  Result := Buffer[BuffPos];

  // if end of line, skip also the LF byte
  if Result=CR then
  begin
       Inc(BuffPos);
       Assert((BuffPos<=Length(Buffer)) and (Buffer[BuffPos]=LF));
       // end of line => compute line numbers
       Inc(LineNum);
  end;
end;

procedure TSourceFile.UnGetChar(const C: Char);
begin
  if C = CR then
  begin
       if BuffPos>1 then Buffer := CR + LF + Buffer
                    else begin
                              Buffer[BuffPos] := LF;
                              Buffer[BuffPos-1] := CR;
                              Dec(BuffPos, 2);
                         end;
       Dec(LineNum);
  end else 
       if BuffPos>0 then Buffer := C + Buffer
                    else begin
                              Buffer[BuffPos] := C;
                              Dec(BuffPos);
                         end;
end;

end.
