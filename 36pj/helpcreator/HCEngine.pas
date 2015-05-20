(******************************************************************************
 *                                                                            *
 *    Delphi Component Help Creator                                           *
 *    version 0.1                                                             *
 *                                                                            *
 *    Semestralni prace z predmetu PJ                                         *
 *    Petr Kadlec <kadlecp2@fel.cvut.cz>                                      *
 *                                                                            *
 ******************************************************************************)

{ The main engine - syntactical analyser + main program flow control } 
unit HCEngine;

interface
uses Classes, SysUtils, HCLexAnalyser, HCSymTable;
{ **************************************************************************** }

type
  ESyntaxError = class(Exception);

procedure Compile(const Src_FN, DstRTF_FN, DstHPJ_FN: string);
procedure RaiseSyntaxError(const ErrMsg: string);

{ **************************** Global variables ****************************** }

var UnitList:    TStringList = nil;     // currently "known" units
    SearchPath:  TStrings    = nil;

{ **************************** Syntax analyser ******************************* }

var Token:  TTokenKind; // current token
    Symbol: TSymbol;    // TSymbol according to the Token, nil if not yet found

{ **************************************************************************** }

implementation
uses Windows, Contnrs, HCInput, HCUnitStack, HCBackEnd;

const DELTA_ALLOC = 1;

{ **************************** Global variables ****************************** }

var ParsedUnits: TStringList = nil;     // list of all already parsed units

{ **************************** Error functions ******************************* }

procedure RaiseSyntaxError(const ErrMsg: string);
begin
     raise ESyntaxError.Create(SourceFile.FormatMsg(ErrMsg));
end;

procedure errXExpectedYFound(const Expected: TTokenKind); overload;
begin
     RaiseSyntaxError(Format('%s expected but %s found', [NameToken(Expected), NameToken(Token, RawText)]));
end;

procedure errXExpectedYFound(const Expected: string); overload;
begin
     RaiseSyntaxError(Format('%s expected but %s found', [Expected, NameToken(Token, RawText)]));
end;

procedure errUndeclared;
begin
     RaiseSyntaxError(Format('Undeclared identifier: ''%s''', [ StrAttrib ]));
end;

{ *************************** Utility functions ****************************** }

function FindUnitSource(const UnitIdent: string): string;
var I: Integer;
begin
     // 1. current directory
     if FileExists(UnitIdent + '.pas') then
     begin
          Result := UnitIdent + '.pas';
          Exit;
     end;

     // 2. search path
     for I:=0 to SearchPath.Count-1 do
     begin
         Result := SearchPath[I] + UnitIdent + '.pas';
         if FileExists(Result) then Exit;
     end;

     raise ESyntaxError.Create('File not found: '''+UnitIdent+'.pas''');
end;

{ **************************** Syntax analyser ******************************* }

type TBufferedToken = class
         Token:    TTokenKind;
         RawText:  string;
         RawAccum: string;
         constructor Create(const T: TTokenKind; const RT, RA: string);
     end;

constructor TBufferedToken.Create(const T: TTokenKind; const RT, RA: string);
begin
     Token := T;
     RawText := RT;
     RawAccum := RA;
end;

var Buffer: TObjectStack = nil;

procedure UnreadToken(const NewToken: TTokenKind; const NewRawText, NewRawAccum: string);
begin
     {OutputDebugString(PChar(Format('UnreadToken: T=%d, RT="%s", SA="%s" => T=%d, RT="%s"',
                                      [Integer(Token), RawText, StrAttrib, Integer(NewToken), NewRawText])));}

     Buffer.Push(TBufferedToken.Create(Token, RawText, RawAccum));
     Token := NewToken;
     RawText := NewRawText;
     RawAccum := NewRawAccum;
     StrAttrib := NewRawText;
end;

procedure ReadToken;
var T: TBufferedToken;
begin
     if Buffer.Count > 0 then
     begin
          T := Buffer.Pop as TBufferedToken;
          Token := T.Token;
          RawText  := T.RawText;
          RawAccum := T.RawAccum;
          StrAttrib := RawText;
          T.Free;
          {OutputDebugString(PChar(Format('ReadToken(BUFFERED): T=%d, RT="%s", SA="%s"',
                                      [Integer(Token), RawText, StrAttrib])));}
     end else
     begin
          RawAccum := RawAccum + ' ' + RawText;
          Token := GetToken;
     end;
     Symbol := nil;
end;

procedure ShiftToken(const Expected: TTokenKind);
begin
     if Token<>Expected then
        errXExpectedYFound(Expected);

     ReadToken;
end;

function CurrentSymbol(const InUnit: string = ''): TSymbol;
var Id, Un: string;
    SaveToken: TTokenKind;
    SaveAccum: array[0..1] of string;
    I: Integer;
begin
     Assert(Token in LexType_Ident);
     if Symbol <> nil then
     begin
          Result := Symbol;
          Exit;
     end;

     SaveToken := Token;
     Id := RawText;
     SaveAccum[0] := RawAccum;
     if UnitList.Find(UpperCase(Id), I) then
     begin
          ReadToken;
          Un := Id;
          Id := RawText;
          SaveAccum[1] := RawAccum;

          ShiftToken(lexDot);
          Id := RawText;

          UnreadToken(lexDot, '.', SaveAccum[1]);
          UnreadToken(SaveToken, Un, SaveAccum[0]);
     end else Un := '';

     Symbol := FindSymbol(Id, Un);
     Result := Symbol;
end;

procedure ShiftTokens(const Expected: array of TTokenKind);
var I: Integer;
begin
     for I:=0 to High(Expected) do
          ShiftToken(Expected[I]);
end;

{ ---------------------------------------------------------------------------- }
procedure ConstExpr; forward;
procedure TypedConstant; forward;
function FieldList: TRecordType; forward;
function Type_(IsPacked: Boolean = False): TType; forward;
procedure ParseUnit(const UnitFileName: string); forward;
{ ---------------------------------------------------------------------------- }

function Ident: string;
begin
     if not (Token in LexType_Ident) then
        errXExpectedYFound(lexIdent);

     Result := StrAttrib;
     ReadToken;
end;

function RestrictedIdent: string;
begin
     if (not (Token in LexType_RestrictedIdent)) then
        errXExpectedYFound(lexIdent);

     Result := StrAttrib;
     ReadToken;
end;

procedure IdentList(const AddTo: TStringList = nil);
var Id: String;
begin
     Id := Ident;
     if AddTo<>nil then AddTo.Add(Id);
     while Token=lexComma do
     begin
          ReadToken;
          Id := Ident;
          if AddTo<>nil then AddTo.Add(Id);
     end;
end;

procedure RestrictedIdentList(const AddTo: TStringList = nil);
var Id: string;
begin
     Id:=RestrictedIdent;
     if AddTo<>nil then AddTo.Add(Id);
     while Token=lexComma do
     begin
          ReadToken;
          Id:=RestrictedIdent;
          if AddTo<>nil then AddTo.Add(Id);
     end;
end;

function QualId: TQualId;
var I: Integer;
begin
     Result.Ident := Ident;
     if UnitList.Find(UpperCase(Result.Ident), I) then
     begin
          Result.UnitId := Result.Ident;
          ShiftToken(lexDot);
          Result.Ident := Ident;
     end else Result.UnitId := '';
end;

function TypeId: TQualId;
begin
     Result := QualId;
end;

procedure UsesClause;
begin
     ShiftToken(lexUSES);
     IdentList(UnitList);
     ShiftToken(lexSemicolon);
end;

procedure SpecialFuncDesignator;
begin
     if Token=lexSYSTEM then
     begin
          ReadToken;
          ShiftToken(lexDot);
     end;

     ShiftToken(lexSpecialFunc);
end;

procedure SetElement;
begin
     ConstExpr;
     if Token=lexDotDot then
     begin
          ReadToken;
          ConstExpr;
     end;
end;

procedure SetConstructor;
begin
     ShiftToken(lexLeftABracket);
     if Token<>lexRightABracket then
     begin
          SetElement;
          while Token=lexComma do
          begin
               ReadToken;
               SetElement;
          end;
     end;
     ShiftToken(lexRightABracket);
end;

procedure Factor;
begin
     case Token of
          lexNIL: ReadToken;
          lexNOT: begin
                       ReadToken;
                       Factor;
                  end;
          lexAt:  begin
                       ReadToken;
                       Factor;
                  end;
          lexSYSTEM,
          lexSpecialFunc: begin
                               // Low, High, SizeOf has a TypeId parameter instead of ConstExpr !
                               {if (CompareText(StrAttrib, 'SizeOf') = 0) or
                                  (CompareText(StrAttrib, 'Low') = 0) or
                                  (CompareText(StrAttrib, 'High') = 0) then
                               begin
                                    SpecialFuncDesignator;
                                    ShiftToken(lexLeftRBracket);
                                    TypeId;
                                    ShiftToken(lexRightRBracket);
                               end else}
                               begin
                                    SpecialFuncDesignator;
                                    ShiftToken(lexLeftRBracket);
                                    ConstExpr;
                                    ShiftToken(lexRightRBracket);
                               end;
                          end;
          lexIntConst,
          lexFloatConst,
          lexStrConst:    ReadToken;
          lexLeftRBracket: begin
                                ReadToken;
                                ConstExpr;
                                ShiftToken(lexRightRBracket);
                           end;
          lexLeftABracket: SetConstructor;
          else begin
                    // choose between "TypeId [ '(' ConstExpr ')' ]" and "QualId"
                    if (CurrentSymbol is TType) then
                    begin
                         TypeId;
                         if Token = lexLeftRBracket then
                         begin
                              ShiftToken(lexLeftRBracket);
                              ConstExpr;
                              ShiftToken(lexRightRBracket);
                         end;
                    end else QualId;
               end;
     end;
end;

procedure Term;
begin
     Factor;
     while Token=lexMulOp do
     begin
          ReadToken;
          Factor;
     end;
end;

procedure SimpleExpression;
begin
     if Token=lexSign then ReadToken;
     Term;
     while Token in [lexSign, lexBinAddOp] do
     begin
          ReadToken;
          Term;
     end;
end;

procedure ConstExpr;
begin
     SimpleExpression;
     while Token=lexRelOp do
     begin
          ReadToken;
          SimpleExpression;
     end;
end;

function SubrangeType: TType;
var SaveAccum: string;
begin
     Result := TType.Create;
     SaveAccum := RawAccum;
     RawAccum := '';
     ConstExpr;
     ShiftToken(lexDotDot);
     ConstExpr;
     Result.Decl := TrimLeft(RawAccum);
     RawAccum := SaveAccum + ' ' + Result.Decl;
end;

function OrdinalType: TType;
var Q: TQualId;
    S,
    SaveAccum: string;
    EnumMembers: TStringList;
    I: Integer;
    EnumMember: TConst;
begin
     Result := nil; // just remove the "return value undefined" warning
     case Token of
          lexSign,
          lexIntConst: Result := SubrangeType;

          lexLeftRBracket: begin
                                Result := TType.Create;
                                SaveAccum := RawAccum;
                                RawAccum := '';
                                // choose between EnumeratedType and SubrangeType
                                ReadToken;
                                if (Token in LexType_Ident) and (CurrentSymbol = nil) then
                                begin
                                     S := Ident;
                                end else
                                begin
                                     ConstExpr;
                                     S := Trim(Copy(RawAccum, 2, Length(RawAccum)));
                                end;
                                case Token of
                                     lexComma: begin
                                                    // EnumeratedType
                                                    ReadToken;
                                                    EnumMembers := TStringList.Create;
                                                    try
                                                      EnumMembers.Add(S);
                                                      IdentList(EnumMembers);
                                                      for I:=0 to EnumMembers.Count-1 do
                                                      begin
                                                           EnumMember := TConst.Create;
                                                           //EnumMember.Typ := Result;
                                                           EnumMember.Ident := EnumMembers[I];
                                                           EnumMember.Decl := EnumMembers[I];
                                                           EnumMember.Imported := True;
                                                           AddSymbol(EnumMember);
                                                      end;
                                                    finally
                                                      EnumMembers.Free;
                                                    end;
                                                    ShiftToken(lexRightRBracket);
                                               end;
                                     lexRightRBracket: begin
                                                            ReadToken;
                                                            if Token = lexDotDot then
                                                            begin
                                                                 // SubrangeType
                                                                 ReadToken;
                                                                 ConstExpr;
                                                            end else
                                                            begin
                                                                 // EnumeratedType with a single member
                                                                 EnumMember := TConst.Create;
                                                                 //EnumMember.Typ := Result;
                                                                 EnumMember.Ident := S;
                                                                 EnumMember.Decl := S;
                                                                 EnumMember.Imported := True;
                                                                 AddSymbol(EnumMember);
                                                            end;
                                                       end;
                                     else errXExpectedYFound(''','' or '')''');
                                end;
                                Result.Decl := TrimLeft(RawAccum);
                                RawAccum := SaveAccum + ' ' + Result.Decl;
                           end;
          else if Token in LexType_Ident then
               begin
                    // choose between TypeId and SubrangeType
                    if not (CurrentSymbol is TType) then
                       // SubrangeType
                       Result := SubrangeType
                    else begin
                              // TypeId
                              Q := TypeId;
                              Result := TEqualityType.Create;
                              TEqualityType(Result).EqualTo := FindSymbol(Q.Ident, Q.UnitId) as TType;
                              if Q.UnitId <> '' then Result.Name := Q.UnitId + '.' + Q.Ident
                                                else Result.Name := Q.Ident;
                              Result.Decl := Result.Name;
                         end;
               end else errXExpectedYFound('Type');
     end;
end;

function ArrayType: TType;
var SaveAccum: string;
begin
     SaveAccum := RawAccum;
     RawAccum := '';
     ShiftToken(lexARRAY);
     if Token = lexLeftABracket then
     begin
          ReadToken;
          OrdinalType;
          while Token=lexComma do
          begin
               ReadToken;
               OrdinalType;
          end;
          ShiftToken(lexRightABracket);
     end;
     ShiftToken(lexOF);
     Type_;
     Result := TType.Create;
     Result.Decl := TrimLeft(RawAccum);
     RawAccum := SaveAccum + ' ' + Result.Decl;
end;

function SetType: TType;
var SaveAccum: string;
begin
     SaveAccum := RawAccum;
     RawAccum := '';
     ShiftTokens([lexSET, lexOF]);
     OrdinalType;
     Result := TType.Create;
     Result.Decl := TrimLeft(RawAccum);
     RawAccum := SaveAccum + ' ' + Result.Decl;
end;

function FileType: TType;
var SaveAccum: string;
begin
     SaveAccum := RawAccum;
     RawAccum := '';
     ShiftToken(lexFILE);
     if Token=lexOF then
     begin
          ReadToken;
          Type_;
     end;
     Result := TType.Create;
     Result.Decl := TrimLeft(RawAccum);
     RawAccum := SaveAccum + ' ' + Result.Decl;
end;

function FieldDecl: TRecordFieldArray;
var FieldIdents: TStringList;
    T: TType;
    I: Integer;
begin
     FieldIdents := TStringList.Create;
     try
       IdentList(FieldIdents);
       ShiftToken(lexColon);
       T := Type_;
       SetLength(Result, FieldIdents.Count);
       for I:=0 to FieldIdents.Count-1 do
       begin
           Result[I] := TRecordField.Create;
           with Result[I] do
           begin
                Ident := FieldIdents[I];
                Typ := T;
           end;
       end;
     finally
       FieldIdents.Free;
     end;
end;

function SimpleFieldList: TRecordFieldArray;
var Len:  Integer;
    List: TRecordFieldArray;
    I:    Integer;
begin
     Len := 0;
     SetLength(Result, 1);
     repeat
           List := FieldDecl;
           if Len+Length(List) >= Length(Result) then SetLength(Result, Len+Length(List) + DELTA_ALLOC);
           for I:=0 to Length(List)-1 do
           begin
                Result[Len] := List[I];
                Inc(Len);
           end;
           if Token=lexSemicolon then ReadToken;
     until not (Token in LexType_Ident);
     SetLength(Result, Len);
end;

function RecVariant: TVarRecPart;
var StoreAccum: string;
begin
     Result := TVarRecPart.Create;
     StoreAccum := RawAccum;
     RawAccum := '';
     ConstExpr;
     while Token=lexComma do
     begin
          ReadToken;
          ConstExpr;
     end;
     Result.SwitchVarValues := TrimLeft(RawAccum);
     RawAccum := StoreAccum + ' ' + Result.SwitchVarValues;
     ShiftTokens([lexColon, lexLeftRBracket]);
     if Token <> lexRightRBracket then Result.Fields := FieldList;
     ShiftToken(lexRightRBracket);
end;

function VariantSection: TVarRecordType;
var Id: TQualId;
    Parts: TVarRecPartArray;
    Len: Integer;
begin
     Result := TVarRecordType.Create;
     ShiftToken(lexCASE);
     Id := TypeId;         // Ident or TypeId
     if Token=lexColon then
     begin
          if Id.UnitId <> '' then errXExpectedYFound(lexOF);
          Result.SwitchVarIdent := Id.Ident;
          ReadToken;
          Result.SwitchVarType := Type_;
     end else
     begin
          Result.SwitchVarType := FindSymbol(Id.Ident, Id.UnitId) as TType;
          if Result.SwitchVarType = nil then errUndeclared;
     end;
     ShiftToken(lexOF);
     Len := 1;
     SetLength(Parts, 1);
     Parts[0] := RecVariant;
     while Token = lexSemicolon do
     begin
          ReadToken;
          if Token = lexEND then Break;

          if Len = Length(Parts) then SetLength(Parts, Len + DELTA_ALLOC);
          Parts[Len] := RecVariant;
          Inc(Len);
     end;
     SetLength(Parts, Len);
     Result.VariantParts := Parts;
end;

function FieldList: TRecordType;
var Fields: TRecordFieldArray;
    Len:    Integer;
    List:   TRecordFieldArray;
    I:      Integer;
begin
     List := nil; // just remove the "uninitialized" warning
     Len := 0;
     while Token in LexType_Ident do
     begin
           List := FieldDecl;
           if Len+Length(List) >= Length(Fields) then SetLength(Fields, Len+Length(List) + DELTA_ALLOC);
           for I:=0 to Length(List)-1 do
           begin
                Fields[Len] := List[I];
                Inc(Len);
           end;
           if Token=lexSemicolon then ReadToken;
     end;
     SetLength(Fields, Len);
     case Token of
          lexCASE: begin
                        Result := VariantSection;
                        Result.Fields := Fields;
                        if Token=lexSemicolon then ReadToken;
                   end;
          lexRightRBracket,
          lexEND:  begin
                        Result := TRecordType.Create;
                        Result.Fields := Fields;
                   end;
          else     Result := nil; //errXExpectedYFound(lexEND); ?
     end;
end;

function RecType: TRecordType;
var SaveAccum: string;
begin
     SaveAccum := RawAccum;
     RawAccum := '';
     ShiftToken(lexRECORD);
     if Token<>lexEND then Result := FieldList
                      else Result := TRecordType.Create;
     ShiftToken(lexEND);
     Result.Decl := TrimLeft(RawAccum);
     RawAccum := SaveAccum + ' ' + Result.Decl;
end;

function StrucType(IsPacked: Boolean = False): TType;
begin
     Result := nil; // just remove the "return value undefined" warning
     if Token = lexPACKED then
     begin
          IsPacked := True;
          ReadToken;
     end;
     case Token of
          lexARRAY:  Result := ArrayType;
          lexSET:    Result := SetType;
          lexFILE:   Result := FileType;
          lexRECORD: begin
                          Result := RecType;
                          if IsPacked then TRecordType(Result).IsPacked := True;
                     end;
          else       if IsPacked then RaiseSyntaxError('PACKED not allowed here')
                                 else errXExpectedYFound('Type');
     end;
end;

function PointerType: TType;
var SaveAccum: string;
begin
     SaveAccum := RawAccum;
     RawAccum := '';
     ShiftToken(lexAccute);
     TypeId;
     Result := TType.Create;
     Result.Decl := TrimLeft(RawAccum);
     RawAccum := SaveAccum + ' ' + Result.Decl;
end;

function StringType: TType;
var SaveAccum: string;
begin
     SaveAccum := RawAccum;
     RawAccum := '';
     ShiftToken(lexSTRING);
     if Token=lexLeftABracket then
     begin
          ReadToken;
          ConstExpr;
          ShiftToken(lexRightABracket);
     end;
     Result := TType.Create;
     Result.Decl := TrimLeft(RawAccum);
     RawAccum := SaveAccum + ' ' + Result.Decl;
end;

procedure RefParameter;
begin
     IdentList;
     if Token=lexColon then
     begin
          ReadToken;
          case Token of
               lexARRAY: begin
                              ReadToken;
                              ShiftToken(lexOF);
                              if Token=lexCONST then ReadToken
                                                else TypeId;
                         end;
               lexSTRING: ReadToken;
               lexFILE:   ReadToken;
               else TypeId;
          end;
     end;
end;

procedure ConstParameter;
var IsArray: Boolean;
begin
     IdentList;
     if Token=lexColon then
     begin
          IsArray:=False;
          ReadToken;
          case Token of
               lexARRAY: begin
                              ReadToken;
                              ShiftToken(lexOF);
                              if Token=lexCONST then ReadToken
                                                else TypeId;
                              IsArray:=True;
                         end;
               lexSTRING: ReadToken;
               lexFILE:   ReadToken;
               else TypeId;
          end;
          if not IsArray then
             if Token=lexEquals then
             begin
                  ReadToken;
                  ConstExpr;
             end;
     end;
end;

procedure ValueParameter;
var IsArray: Boolean;
begin
     IdentList;
     if Token=lexColon then
     begin
          IsArray:=False;
          ReadToken;
          case Token of
               lexARRAY: begin
                              ReadToken;
                              ShiftToken(lexOF);
                              if Token=lexCONST then ReadToken
                                                else TypeId;
                              IsArray:=True;
                         end;
               lexSTRING: ReadToken;
               lexFILE:   ReadToken;
               else TypeId;
          end;
          if not IsArray then
             if Token=lexEquals then
             begin
                  ReadToken;
                  ConstExpr;
             end;
     end;
end;

procedure FormalParam;
begin
     case Token of
          lexVAR,
          lexOUT: begin
                       ReadToken;
                       RefParameter;
                  end;
          lexCONST: begin
                         ReadToken;
                         ConstParameter;
                    end;
          else ValueParameter;
     end;
end;

procedure FormalParameters;
begin
     ShiftToken(lexLeftRBracket);
     if Token<>lexRightRBracket then
     begin
          FormalParam;
          while Token=lexSemicolon do
          begin
               ReadToken;
               FormalParam;
          end;
     end;
     ShiftToken(lexRightRBracket);
end;

function ProcedureHeading: TProc;
var SaveAccum: string;
begin
     SaveAccum := RawAccum;
     RawAccum := '';
     Result := TProc.Create;
     Result.ProcKind := pkProcedure;
     ShiftToken(lexPROCEDURE);
     Result.Ident := Ident;
     if Token=lexLeftRBracket then FormalParameters;
     Result.Decl := TrimLeft(RawAccum);
     RawAccum := SaveAccum + ' ' + Result.Decl;
end;

function FunctionHeading: TProc;
var SaveAccum: string;
begin
     SaveAccum := RawAccum;
     RawAccum := '';
     Result := TProc.Create;
     Result.ProcKind := pkFunction;
     ShiftToken(lexFUNCTION);
     Result.Ident := Ident;
     if Token=lexLeftRBracket then FormalParameters;
     ShiftToken(lexColon);
     TypeId;
     Result.Decl := TrimLeft(RawAccum);
     RawAccum := SaveAccum + ' ' + Result.Decl;
end;

procedure ProcTypeHeading;
begin
     ShiftToken(lexPROCEDURE);
     if Token in LexType_Ident then ReadToken;
     if Token = lexLeftRBracket then FormalParameters;
end;

procedure FuncTypeHeading;
begin
     ShiftToken(lexFUNCTION);
     if Token in LexType_Ident then ReadToken;
     if Token = lexLeftRBracket then FormalParameters;
     ShiftToken(lexColon);
     TypeId;
end;

procedure DirectiveList;
var SaveAccum: string;
begin
     while True do
     begin
          if Token = lexSemicolon then
          begin
               SaveAccum := RawAccum;
               ReadToken;
               if not (Token in LexType_Directive) then
               begin
                    UnreadToken(lexSemicolon, ';', SaveAccum);
                    Exit;
               end;
          end else
              if not (Token in LexType_Directive) then Exit;

          ReadToken;
     end;
end;

function ProcedureType: TType;
var SaveAccum: string;
begin
     SaveAccum := RawAccum;
     RawAccum := '';
     case Token of
          lexPROCEDURE: ProcTypeHeading;
          lexFUNCTION:  FuncTypeHeading;
          else errXExpectedYFound('''PROCEDURE'' or ''FUNCTION''');
     end;
     if Token=lexOF then
     begin
          ReadToken;
          ShiftToken(lexOBJECT);
     end;
     DirectiveList();
     Result := TProcType.Create;
     Result.Decl := TrimLeft(RawAccum);
     RawAccum := SaveAccum + ' ' + Result.Decl;
end;

function ClassRefType: TType;
var SaveAccum: string;
begin
     SaveAccum := RawAccum;
     RawAccum := '';
     ShiftTokens([lexCLASS, lexOF]);
     TypeId;
     Result := TType.Create;
     Result.Decl := TrimLeft(RawAccum);
     RawAccum := SaveAccum + ' ' + Result.Decl;
end;

function Type_(IsPacked: Boolean = False): TType;
var Q: TQualId;
begin
     case Token of
          lexTYPE: begin
                        ReadToken;
                        Q := TypeId;
                        Result := TEqualityType.Create;
                        TEqualityType(Result).EqualTo := FindSymbol(Q.Ident, Q.UnitId) as TType;
                   end;
          lexPACKED,
          lexARRAY,
          lexSET,
          lexFILE,
          lexRECORD:   Result := StrucType(IsPacked);
          lexAccute:   Result := PointerType;
          lexSTRING:   Result := StringType;
          lexPROCEDURE,
          lexFUNCTION: Result := ProcedureType;
          lexCLASS:    Result := ClassRefType;
          else         Result := OrdinalType;
     end;
end;

procedure BeganArrayConstant;
begin
     //ShiftToken(lexLeftRBracket);
     TypedConstant;
     while Token=lexComma do
     begin
          ReadToken;
          TypedConstant;
     end;
     ShiftToken(lexRightRBracket);
end;

procedure RecordFieldConstant;
begin
     Ident;
     ShiftToken(lexColon);
     TypedConstant;
end;

procedure BeganRecordConstant;
begin
     //ShiftToken(lexLeftRBracket);
     RecordFieldConstant;
     while Token = lexSemicolon do
     begin
          ReadToken;
          RecordFieldConstant;
     end;
     ShiftToken(lexRightRBracket);
end;

procedure TypedConstant;
var FirstToken: TTokenKind;
    FirstRawText: string;
    SaveAccum: string;
begin
     // ArrayConstant versus RecordConstant
     case Token of
          lexLeftRBracket: begin
                             ReadToken;
                             if Token in LexType_Ident then
                             begin
                                FirstToken := Token;
                                FirstRawText := RawText;
                                SaveAccum := RawAccum;

                                ReadToken;

                                if Token = lexColon then
                                begin
                                     UnreadToken(FirstToken, FirstRawText, SaveAccum);
                                     BeganRecordConstant;
                                end else
                                begin
                                     UnreadToken(FirstToken, FirstRawText, SaveAccum);
                                     BeganArrayConstant;
                                end;
                             end else BeganArrayConstant;
                           end;
          else ConstExpr;
     end;
end;

function ConstDecl: TConst;
begin
     RawAccum := '';
     Result := TConst.Create;
     Result.Ident := Ident;
     //Result.Typ := nil;
     case Token of
          lexEquals: begin
                          ReadToken;
                          ConstExpr;
                     end;
          lexColon:  begin
                          ReadToken;
                          //Result.Typ := Type_;
                          Type_;
                          ShiftToken(lexEquals);
                          TypedConstant;
                     end;
          else errXExpectedYFound(lexEquals);
     end;
     Result.Decl := 'const ' + TrimLeft(RawAccum) + ';';
end;

procedure ConstSection;
begin
     ShiftToken(lexCONST);
     repeat
           AddSymbol(ConstDecl);
           ShiftToken(lexSemicolon);
     until not (Token in LexType_Ident);
end;

procedure ExportDecl;
begin
     QualId;
     case Token of
          lexLeftRBracket: FormalParameters;
          lexINDEX:        begin
                                ReadToken;
                                ConstExpr;
                           end;
     end;
     if Token=lexNAME then
     begin
          ReadToken;
          ConstExpr;
          if Token=lexRESIDENT then ReadToken;
     end;
end;

procedure ExportsClause;
begin
     ShiftToken(lexEXPORTS);

     ExportDecl;
     while Token = lexComma do
     begin
          ReadToken;
          ExportDecl;
     end;

     ShiftToken(lexSemicolon);
end;

function ResourceStringDecl: TResString;
begin
     Result := TResString.Create;
     RawAccum := '';
     Result.Ident := Ident;
     ShiftToken(lexEquals);
     ConstExpr;
     ShiftToken(lexSemicolon);
     Result.Decl := TrimLeft(RawAccum);
end;

procedure ResourceStringSection;
begin
     ShiftToken(lexRESOURCESTRING);
     repeat
           AddSymbol(ResourceStringDecl);
     until not (Token in LexType_Ident);
end;

procedure ExternalClause;
begin
     ShiftToken(lexEXTERNAL);
     if Token <> lexSemicolon then
     begin
          ConstExpr;
          case Token of
               lexNAME: begin
                             ReadToken;
                             ConstExpr;
                        end;
               lexINDEX: begin
                              ReadToken;
                              ConstExpr;
                         end;
          end;
     end;
     if Token = lexSemicolon then ReadToken;
end;

procedure ExportedHeading;
var Sym: TProc;
begin
     Sym := nil; // just remove the "uninitialized" warning
     RawAccum := '';
     case Token of
          lexPROCEDURE: Sym := ProcedureHeading;
          lexFUNCTION:  Sym := FunctionHeading;
          else          errXExpectedYFound('Declaration');
     end;
     DirectiveList;
     ShiftToken(lexSemicolon);  
     if Token = lexEXTERNAL then ExternalClause;
     Sym.Decl := TrimLeft(RawAccum);
     AddSymbol(Sym);
end;

procedure VarDecl;
var Idents: TStringList;
    T: TType;
    V: TVariable;
    I: Integer;
begin
     Idents := TStringList.Create;
     try
       IdentList(Idents);
       ShiftToken(lexColon);
       T := Type_;
       case Token of
            lexABSOLUTE: begin
                              ReadToken;
                              ConstExpr;
                         end;
            lexEquals:   begin
                              ReadToken;
                              TypedConstant;
                         end;
       end;
       for I:=0 to Idents.Count-1 do
       begin
           V := TVariable.Create;
           with V do
           begin
                //Typ := T;
                Ident := Idents[I];
                Decl := 'var '+Ident+': '+T.Decl+';';
           end;
           AddSymbol(V);
       end;
     finally
       Idents.Free;
     end;
end;

procedure VarSection;
begin
     if not (Token in [lexVAR, lexTHREADVAR]) then errXExpectedYFound('Declaration');
     ReadToken;
     repeat
           VarDecl;
           ShiftToken(lexSemicolon);
     until not (Token in LexType_Ident);
end;

function PropertyInterface: TType;
var Q: TQualId;
begin
     if Token = lexLeftABracket then
     begin
          ReadToken;
          FormalParam;
          while Token = lexSemicolon do
          begin
               ReadToken;
               FormalParam;
          end;
          ShiftToken(lexRightABracket);
     end;
     ShiftToken(lexColon);
     Q := TypeId;
     Result := FindSymbol(Q.Ident, Q.UnitId) as TType;
end;

function DIntfProperty: TObjProperty;
var SaveAccum: string;
begin
     SaveAccum := RawAccum;
     RawAccum := '';
     Result := TObjProperty.Create;
     ShiftToken(lexPROPERTY);
     Result.Name := Ident;

     if Token in [lexLeftABracket, lexColon] then Result.Typ := PropertyInterface;

     if Token = lexREADONLY then ReadToken
                            else Result.CanRead := True;
     if Token = lexWRITEONLY then ReadToken
                             else Result.CanWrite := True;
     if Token = lexDISPID then
     begin
          ReadToken;
          ConstExpr;
     end;

     ShiftToken(lexSemicolon);
     if Token = lexDEFAULT then
     begin
          ReadToken;
          ShiftToken(lexSemicolon);
     end;

     Result.Decl := RawAccum;
     RawAccum := SaveAccum + ' ' + Result.Decl;
end;

function DIntfMethod: TObjMethod;
var SaveAccum: string;
    P: TProc;
begin
     SaveAccum := RawAccum;
     RawAccum := '';
     Result := TObjMethod.Create;

     P:=nil; // just remove the "uninitialized" warning
     case Token of
          lexPROCEDURE: P := ProcedureHeading;
          lexFUNCTION:  P := FunctionHeading;
          else          errXExpectedYFound(lexEND);
     end;

     Result.Name := P.Ident;
     P.Free;

     DirectiveList;
     ShiftToken(lexSemicolon); 

     if Token = lexDISPID then
     begin
          ReadToken;
          ConstExpr;
          ShiftToken(lexSemicolon);
     end;

     Result.Decl := RawAccum;
     RawAccum := SaveAccum + ' ' + Result.Decl;
end;

function IntfProperty: TObjProperty;
var SaveAccum: string;
begin
     SaveAccum := RawAccum;
     RawAccum := '';
     Result := TObjProperty.Create;
     ShiftToken(lexPROPERTY);
     Result.Name := Ident;

     if Token in [lexLeftABracket, lexColon] then Result.Typ := PropertyInterface;

     Result.Decl := RawAccum + ';';
     RawAccum := SaveAccum + ' ' + Result.Decl;

     if Token = lexREAD then
     begin
          Result.CanRead := True;
          ReadToken;
          Ident;
     end;
     if Token = lexWRITE then
     begin
          Result.CanWrite := True;
          ReadToken;
          Ident;
     end;

     ShiftToken(lexSemicolon);
     if Token = lexDEFAULT then
     begin
          ReadToken;
          ShiftToken(lexSemicolon);
     end;
end;

function IntfMethod: TObjMethod;
var SaveAccum: string;
    P: TProc;
begin
     SaveAccum := RawAccum;
     RawAccum := '';
     Result := TObjMethod.Create;

     P:=nil; // just remove the "uninitialized" warning
     case Token of
          lexPROCEDURE: P := ProcedureHeading;
          lexFUNCTION:  P := FunctionHeading;
          else          errXExpectedYFound(lexEND);
     end;

     Result.Name := P.Ident;
     P.Free;

     DirectiveList;
     ShiftToken(lexSemicolon);  

     Result.Decl := RawAccum;
     RawAccum := SaveAccum + ' ' + Result.Decl;
end;

function DispinterfaceType: TInterfaceType;
var SaveAccum: string;
    MetLen,
    PropLen: Integer;
begin
     ShiftToken(lexDISPINTERFACE);
     Result := TInterfaceType.Create;
     Result.Kind := okDispinterface;
     if Token <> lexLeftABracket then
     begin
          // forward declaration (type TDIntf = dispinterface;)
          Result.ForwardDecl := True;
          Exit;
     end;
     ReadToken;
     SaveAccum := RawAccum;
     RawAccum := '';
     ConstExpr;
     Result.GUID := RawAccum;
     RawAccum := SaveAccum + ' ' + Result.GUID;

     MetLen := 0;
     PropLen := 0;

     while Token <> lexEND do
     begin
          if Token = lexPROPERTY then
          begin
               if Length(Result.Properties) = PropLen then SetLength(Result.Properties, PropLen + DELTA_ALLOC);
               Result.Properties[PropLen] := DIntfProperty();
               Inc(PropLen);
          end else
          begin
               DIntfMethod;
               if Length(Result.Methods) = MetLen then SetLength(Result.Methods, MetLen + DELTA_ALLOC);
               Result.Methods[MetLen] := DIntfMethod();
               Inc(MetLen);
          end;
     end;
     SetLength(Result.Properties, PropLen);
     SetLength(Result.Methods, MetLen);
     ReadToken; // lexEND
end;

function InterfaceType: TInterfaceType;
var SaveAccum: string;
    MetLen,
    PropLen: Integer;
begin
     Result := TInterfaceType.Create;
     Result.Kind := okInterface;
     ShiftToken(lexINTERFACE);
     if Token = lexLeftRBracket then
     begin
          ReadToken;
          Result.SuperClass := TypeId().Ident;
          ShiftToken(lexRightRBracket);
     end;
     if Token = lexLeftABracket then
     begin
          ReadToken;
          SaveAccum := RawAccum;
          RawAccum := '';
          ConstExpr;
          Result.GUID := RawAccum;
          RawAccum := SaveAccum + ' ' + Result.GUID;
          ShiftToken(lexRightABracket);
     end;
     if not (Token in [lexPROCEDURE, lexFUNCTION, lexPROPERTY, lexEND]) then
     begin
          // forward declaration (type TIntf = interface;)
          Result.ForwardDecl := True;
          Exit;
     end;

     MetLen := 0;
     PropLen := 0;

     while Token <> lexEND do
     begin
          if Token = lexPROPERTY then
          begin
               if Length(Result.Properties) = PropLen then SetLength(Result.Properties, PropLen + DELTA_ALLOC);
               Result.Properties[PropLen] := IntfProperty();
               Inc(PropLen);
          end else
          begin
               if Length(Result.Methods) = MetLen then SetLength(Result.Methods, MetLen + DELTA_ALLOC);
               Result.Methods[MetLen] := IntfMethod();
               Inc(MetLen);
          end;
     end;
     SetLength(Result.Properties, PropLen);
     SetLength(Result.Methods, MetLen);
     ReadToken; // lexEND
end;

function Heritage: string;
begin
     ShiftToken(lexLeftRBracket);

     Result := QualId().Ident;
     while Token = lexComma do
     begin
          ReadToken;
          QualId;
     end;

     ShiftToken(lexRightRBracket);
end;

function ObjField: TObjFieldArray;
var Ids: TStringList;
    T:   TType;
    I:   Integer;
begin
     Ids := TStringList.Create;
     try
       RestrictedIdentList(Ids);
       ShiftToken(lexColon);
       T := Type_;
       ShiftToken(lexSemicolon);
       SetLength(Result, Ids.Count);
       for I:=0 to Ids.Count-1 do
       begin
            Result[I] := TObjField.Create;
            with Result[I] do
            begin
                 Name := Ids[I];
                 Decl := Name+': '+T.Decl+';';
            end;
       end;
     finally
       Ids.Free;
     end;
end;

procedure PropertySpecifiers(Prop: TObjProperty);
begin
     if Token = lexINDEX then
     begin
          ReadToken;
          ConstExpr;
     end;

     if Token = lexREAD then
     begin
          Prop.CanRead := True;
          ReadToken;
          Ident;
     end;
     if Token = lexWRITE then
     begin
          Prop.CanWrite := True;
          ReadToken;
          Ident;
     end;

     if Token = lexSTORED then
     begin
          ReadToken;
          Ident;
          //ConstExpr;
     end;

     if Token = lexDEFAULT then
     begin
          ReadToken;
          ConstExpr;
     end else
      if Token = lexNODEFAULT then
      begin
           ReadToken;
      end;

     if Token = lexIMPLEMENTS then
     begin
          ReadToken;
          TypeId;
     end;
end;

function ObjProperty: TObjProperty;
var SaveAccum: string;
begin
     SaveAccum := RawAccum;
     RawAccum := '';
     Result := TObjProperty.Create;
     ShiftToken(lexPROPERTY);
     Result.Name := Ident();
     if (Token = lexLeftABracket) or (Token = lexColon) then
     begin
          Result.Typ := PropertyInterface;
          Result.Decl := RawAccum + ';';
          RawAccum := SaveAccum + ' ' + Result.Decl;
          PropertySpecifiers(Result);
     end else
     begin
          Result.Decl := RawAccum + ';';
          RawAccum := SaveAccum + ' ' + Result.Decl;
          if Token in [lexINDEX, lexREAD, lexWRITE, lexSTORED, lexDEFAULT, lexNODEFAULT] then PropertySpecifiers(Result);
     end;
     ShiftToken(lexSemicolon);
     if Token = lexDEFAULT then
     begin
          ReadToken;
          ShiftToken(lexSemicolon);
     end;
end;

procedure ObjDirectiveList;
begin
     while Token in [lexObjDirective] + LexType_Directive do
     begin
          ReadToken;
          ShiftToken(lexSemicolon);
     end;
end;

function Method: TObjMethod;
var SaveAccum: string;
begin
     Result := TObjMethod.Create;
     SaveAccum := RawAccum;
     RawAccum := '';
     if Token = lexCLASS then
     begin
          ReadToken;
          if not (Token in [lexPROCEDURE, lexFUNCTION]) then
             errXExpectedYFound('PROCEDURE or FUNCTION');
     end;
     case Token of
          lexCONSTRUCTOR,
          lexDESTRUCTOR: begin
                              ReadToken;
                              Result.Name := Ident;
                              if Token = lexLeftRBracket then FormalParameters;
                              ShiftToken(lexSemicolon);
                              if Token = lexMESSAGE then
                              begin
                                   ReadToken;
                                   ConstExpr;
                                   ShiftToken(lexSemicolon);
                              end;
                              ObjDirectiveList;
                         end;
          lexPROCEDURE: begin
                             ReadToken;
                             // choose between MethodHeading and "TypeId `.` Ident `=` Ident `;`"

                             if CurrentSymbol is TType then
                             begin
                                  TypeId;
                                  ShiftToken(lexDot);
                                  Result.Name := Ident;
                                  ShiftToken(lexEquals);
                                  Ident;
                                  ShiftToken(lexSemicolon);
                             end else
                             begin
                                  Result.Name := Ident;
                                  if Token = lexLeftRBracket then FormalParameters;
                                  ShiftToken(lexSemicolon);
                                  if Token = lexMESSAGE then
                                  begin
                                       ReadToken;
                                       ConstExpr;
                                       ShiftToken(lexSemicolon);
                                  end;
                                  ObjDirectiveList;
                             end;
                        end;
          lexFUNCTION:  begin
                             ReadToken;
                             if CurrentSymbol is TType then
                             begin
                                  TypeId;
                                  ShiftToken(lexDot);
                                  Result.Name := Ident;
                                  ShiftToken(lexEquals);
                                  Ident;
                                  ShiftToken(lexSemicolon);
                             end else
                             begin
                                  Result.Name := Ident;
                                  if Token = lexLeftRBracket then FormalParameters;
                                  ShiftToken(lexColon);
                                  TypeId;
                                  ShiftToken(lexSemicolon);
                                  if Token = lexMESSAGE then
                                  begin
                                       ReadToken;
                                       ConstExpr;
                                       ShiftToken(lexSemicolon);
                                  end;
                                  ObjDirectiveList;
                             end;
                        end;
          else          errXExpectedYFound(lexEND);
     end;
     Result.Decl := RawAccum;
     RawAccum := SaveAccum + ' ' + Result.Decl;
end;

function ObjectType: TClassType;
var Lens:    array[TObjSectionVisibility] of Integer;
    Section: TObjSectionVisibility;
    Fields:  TObjFieldArray;
    I:       Integer;
begin
     Fields := nil;  // just to remove the "uninitialized" warning

     ShiftToken(lexOBJECT);

     Result := TClassType.Create;
     if Token = lexLeftRBracket then Result.SuperClass := Heritage;

     Result.Kind := okObject;

     for Section := Low(Lens) to High(Lens) do Lens[Section] := 0;

     Section := svPublic;
     while True do // break on lexEND - see below
     begin
          while Token in LexType_RestrictedIdent do
          begin
               Fields := ObjField();
               if Length(Result.Sections[Section])+Length(Fields) >= Lens[Section] then SetLength(Result.Sections[Section], Lens[Section] + Length(Fields) + DELTA_ALLOC);
               for I:=0 to Length(Fields)-1 do
               begin
                    Result.Sections[Section][Lens[Section]] := Fields[I];
                    Inc(Lens[Section]);
               end;
          end;
          while Token in [lexPROPERTY, lexFUNCTION, lexPROCEDURE, lexCONSTRUCTOR, lexDESTRUCTOR] do
          begin
               if Length(Result.Sections[Section]) = Lens[Section] then SetLength(Result.Sections[Section], Lens[Section] + DELTA_ALLOC);
               if Token = lexPROPERTY then
                   Result.Sections[Section][Lens[Section]] := ObjProperty()
               else
                   Result.Sections[Section][Lens[Section]] := Method();
               Inc(Lens[Section]);
          end;
          case Token of
               lexPUBLIC: begin
                               ReadToken;
                               Section := svPublic;
                          end;
               lexPRIVATE: begin
                                ReadToken;
                                Section := svPrivate;
                           end;
               lexPROTECTED: begin
                                  ReadToken;
                                  Section := svProtected;
                             end;
               lexEND:  Break;
               else     begin
                             if Token in LexType_Ident then
                                RaiseSyntaxError('Field definition not allowed after methods or properties')
                             else
                                errXExpectedYFound(lexEND);
                        end;
          end;
     end;

     for Section := Low(Lens) to High(Lens) do
         SetLength(Result.Sections[Section], Lens[Section]);

     ReadToken; // lexEND
end;

function BeganClassType: TClassType;
var Lens:    array[TObjSectionVisibility] of Integer;
    Section: TObjSectionVisibility;
    Fields:  TObjFieldArray;
    I:       Integer;
begin
     Fields := nil;  // just to remove the "uninitialized" warning
     Result := nil; 

     if Token = lexLeftRBracket then
     begin
          Result := TClassType.Create;
          Result.Kind := okClass;
          Result.SuperClass := Heritage();
     end;
     if Token = lexSemicolon then
     begin
          if Result = nil then
          begin
               // forward declaration (type TCls = class(TParent);)
               Result := TClassType.Create;
               Result.Kind := okClass;
               Result.ForwardDecl := True;
          end; // else simple subclassing (type TChild = class(TParent);)
          Exit;
     end;
     if Result = nil then
     begin
          Result := TClassType.Create;
          Result.Kind := okClass;
     end;

     for Section := Low(Lens) to High(Lens) do Lens[Section] := 0;

     Section := svPublished;
     while True do // break on lexEND - see below
     begin
          while Token in LexType_RestrictedIdent do
          begin
               Fields := ObjField();
               if Length(Result.Sections[Section])+Length(Fields) >= Lens[Section] then SetLength(Result.Sections[Section], Lens[Section] + Length(Fields) + DELTA_ALLOC);
               for I:=0 to Length(Fields)-1 do
               begin
                    Result.Sections[Section][Lens[Section]] := Fields[I];
                    Inc(Lens[Section]);
               end;
          end;
          while Token in [lexPROPERTY, lexCLASS, lexFUNCTION, lexPROCEDURE, lexCONSTRUCTOR, lexDESTRUCTOR] do
          begin
               if Length(Result.Sections[Section]) = Lens[Section] then SetLength(Result.Sections[Section], Lens[Section] + DELTA_ALLOC);
               if Token = lexPROPERTY then
                   Result.Sections[Section][Lens[Section]] := ObjProperty()
               else
                   Result.Sections[Section][Lens[Section]] := Method();
               Inc(Lens[Section]);
          end;
          case Token of
               lexPUBLIC: begin
                               ReadToken;
                               Section := svPublic;
                          end;
               lexPRIVATE: begin
                                ReadToken;
                                Section := svPrivate;
                           end;
               lexPROTECTED: begin
                                  ReadToken;
                                  Section := svProtected;
                             end;
               lexPUBLISHED: begin
                                  ReadToken;
                                  Section := svPublished;
                             end;
               lexAUTOMATED: begin
                                  ReadToken;
                                  Section := svAutomated;
                             end;
               lexEND:  Break;
               else     begin
                             if Token in LexType_Ident then
                                RaiseSyntaxError('Field definition not allowed after methods or properties')
                             else
                                errXExpectedYFound(lexEND);
                        end;
          end;
     end;

     for Section := Low(Lens) to High(Lens) do
         SetLength(Result.Sections[Section], Lens[Section]);
         
     ReadToken; // lexEND
end;

procedure TypeDecl;
var Id: string;
    T:  TType;
begin
     Id:=Ident;

     // the identifier is known from this point (to allow: tlink = class data: tobject; next: tcls; end;) 
     T := TType.Create;
     T.Ident := Id;
     T.Name := Id;
     T.ForwardDecl := True;
     AddSymbol(T);

     T := nil;

     ShiftToken(lexEquals);
     RawAccum := '';
     case Token of
          lexOBJECT:        T := ObjectType;
          lexINTERFACE:     T := InterfaceType;
          lexDISPINTERFACE: T := DispInterfaceType;
          lexPACKED:        begin
                                 ReadToken;
                                 case Token of
                                      lexOBJECT: T := ObjectType;
                                      lexCLASS:  begin
                                                      ReadToken;
                                                      T := BeganClassType;
                                                 end;
                                      lexARRAY,
                                      lexSET,
                                      lexFILE,
                                      lexRECORD: T := Type_(True);
                                      else RaiseSyntaxError('PACKED not allowed here');
                                 end;
                            end;
          lexCLASS:         begin
                                 ReadToken;
                                 if Token = lexOF then
                                 begin
                                      // class reference type
                                      ReadToken;
                                      TypeId;
                                      T := TType.Create;
                                 end else
                                 begin
                                      // class type (now after the "class" word)
                                      T := BeganClassType;
                                 end;
                            end;
          else              T := Type_;
     end;
     if T<>nil then
     begin
          T.Ident := Id;
          T.Name := Id;
          T.Decl := TrimLeft(RawAccum);
          AddSymbol(T);
     end;
end;

procedure TypeSection;
begin
     ShiftToken(lexTYPE);
     repeat
           TypeDecl;
           ShiftToken(lexSemicolon);
     until not (Token in LexType_Ident);
end;

procedure InterfaceDecl;
begin
     case Token of
          lexCONST:          ConstSection;
          lexTYPE:           TypeSection;
          lexVAR,
          lexTHREADVAR:      VarSection;
          lexPROCEDURE,
          lexFUNCTION:       ExportedHeading;
          lexRESOURCESTRING: ResourceStringSection;
          lexEXPORTS:        ExportsClause;
          else               errXExpectedYFound('Declaration');
     end;
end;

procedure UnitHeading;
var S: string;
    I, P: Integer;
    ToParse: TStringList;
    SaveImported: Boolean;
begin
     ShiftToken(lexUNIT);
     CurrentUnit := Ident;
     S := UpperCase(CurrentUnit);
     UnitList.Add(S);
     UnitList.Add('SYSTEM');
     ParsedUnits.Add(S);
     ShiftTokens([lexSemicolon, lexINTERFACE]);
     if Token=lexUSES then UsesClause;

     ToParse := TStringList.Create;
     try
       for I:=0 to UnitList.Count-1 do
       begin
          S := UpperCase(UnitList[I]);
          if not ParsedUnits.Find(S, P) then
          begin
               OutputDebugString(PChar(Format('Unit "%s" uses "%s"', [CurrentUnit, UnitList[I]])));
               ToParse.Add(S);
               //ParsedUnits.Add(S);
          end;
       end;

       UnitList.Sorted := True;

       SaveImported := AllImported;
       AllImported := True;

       for I:=0 to ToParse.Count-1 do
           ParseUnit(FindUnitSource(ToParse[I]));

       AllImported := SaveImported;
     finally
       ToParse.Free;
     end;

     while Token<>lexIMPLEMENTATION do
           InterfaceDecl;
end;

{ **************************************************************************** }

var LastUnit: string;

procedure ParseUnit(const UnitFileName: string);
begin
     OutputDebugString(PChar(Format('Parsing unit "%s"', [UnitFileName])));
     UnitStack_Push;

     UnitList := TStringList.Create;
     try
       HCLexAnalyser.SourceFile := TSourceFile.Create(UnitFileName);

       try
         ReadToken;
         UnitHeading;
       finally
         FreeAndNil(HCLexAnalyser.SourceFile);
       end;

     finally
       FreeAndNil(UnitList);
     end;

     LastUnit := CurrentUnit;
     UnitStack_Pop;
     OutputDebugString(PChar(Format('Unit "%s" parsed OK', [UnitFileName])));
end;

{ **************************************************************************** }

procedure Compile(const Src_FN, DstRTF_FN, DstHPJ_FN: string);
begin
     // phase 1. Parse the unit (and dependent units)
     //ParsedUnits := TStringList.Create;
     try
       ParsedUnits.Sorted := True;
       ParsedUnits.Duplicates := dupIgnore;

       ParseUnit(Src_FN);
     finally
       //FreeAndNil(ParsedUnits);
     end;

     // phase 2. Compile the help text
     if Assigned(OnProgress) then OnProgress('', 'Creating RTF file');
     CreateRTF(DstRTF_FN, LastUnit);

     // phase 3. Create the help project
     if Assigned(OnProgress) then OnProgress('', 'Creating HPJ file');
     CreateHPJ(DstHPJ_FN, DstRTF_FN, LastUnit);
end;

initialization
  Buffer:=TObjectStack.Create;
  ParsedUnits := TStringList.Create;
finalization
  FreeAndNil(ParsedUnits);
  FreeAndNil(Buffer);
end.
