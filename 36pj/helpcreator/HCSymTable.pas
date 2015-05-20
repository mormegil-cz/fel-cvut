(******************************************************************************
 *                                                                            *
 *    Delphi Component Help Creator                                           *
 *    version 0.1                                                             *
 *                                                                            *
 *    Semestralni prace z predmetu PJ                                         *
 *    Petr Kadlec <kadlecp2@fel.cvut.cz>                                      *
 *                                                                            *
 ******************************************************************************)

{ Symbol table entry types and functions }
unit HCSymTable;

interface

type
  TQualId = record
                  UnitId: string;
                  Ident:  string;
            end;

  TSymbol = class
                 Ident: string;
                 Decl:  string;

                 ForwardDecl: Boolean; // true = forward declaration, will be defined later
                 Imported: Boolean;    // true = do not include into the help file (imported from outside, member of enumeration, etc.)

                 ForwardedTo: TSymbol;

                 function GetForwardedTo: TSymbol;
                 constructor CreateEx(const I, D: string; const Imp: Boolean = False);
            end;

  TType = class(TSymbol)
               Name: string; // = Ident if named type, = Decl if "inline" (??? IS THIS GOOD IDEA ???)

               constructor CreateEx(const I, D, N: string; const Imp: Boolean = False);
          end;

  TVariable = class(TSymbol)
              end;

  TConst = class(TSymbol)
                //Typ: TType;
           end;

  TResString = class(TConst)
               end;

  TProcType = class(TType)
              end;

  TEqualityType = class(TType)
                       EqualTo: TType;
                  end;

  TProcKind = (pkFunction, pkProcedure);
  TProc = class(TSymbol)
               ProcKind: TProcKind;
          end;

  {TEnumeratedType = class(TType)
                         Values: array of TConst; // define them as normal consts as well!
                    end;}

  TRecordField = class
                      Ident: string;
                      Typ:   TType;
                 end;

  TRecordFieldArray = array of TRecordField;
  TRecordType = class(TType)
                     IsPacked: Boolean;
                     Fields: TRecordFieldArray;
                end;

  TVarRecPart = class
                     SwitchVarValues: string;
                     Fields: TRecordType;
                end;

  TVarRecPartArray = array of TVarRecPart;
  TVarRecordType = class(TRecordType)
                        SwitchVarIdent: string;
                        SwitchVarType:  TType;
                        VariantParts:   TVarRecPartArray;
                   end;

  TObjMember = class
                   Name: string;
                   Decl: string;
               end;

  TObjField = class(TObjMember)
              end;

  //TObjMethodKind = (mkFunction, mkProcedure, mkConstructor, mkDestructor);
  TObjMethod = class(TObjMember)
                    //Kind: TObjMethodKind;
               end;

  TObjProperty = class(TObjMember)
                    CanRead, CanWrite: Boolean;
                    Typ: TType;
                 end;

  TObjMemberArray = array of TObjMember;
  TObjFieldArray = array of TObjField;
  TObjMethodArray = array of TObjMethod;
  TObjPropertyArray = array of TObjProperty;

  TObjKind = (okObject, okClass, okInterface, okDispinterface);
  TObjSectionVisibility = (svPublic, svPrivate, svProtected, svPublished, svAutomated);
  TObjectiveType = class(TType)
                        Kind:       TObjKind;
                        SuperClass: string;  // direct superclass
                   end;

  TInterfaceType = class(TObjectiveType)
                        GUID:       string;
                        Methods:    TObjMethodArray;
                        Properties: TObjPropertyArray;
                   end;

  TClassType = class(TObjectiveType)
                    Sections: array[TObjSectionVisibility] of TObjMemberArray;
               end;

// ----------------------------------------------------------------------------

var CurrentUnit: string = '?';

    AllImported: Boolean = False; // True = all symbols for AddSymbol are imported

    OnProgress: function(const UnitName, SymbolName: string): Boolean of object;

function FindSymbol(const Ident: string; const InUnit: string = ''): TSymbol;
procedure AddSymbol(const Symbol: TSymbol; const ToUnit: string = '');

// ----------------------------------------------------------------------------

type PUnitSymTable = ^TUnitSymTable;
     TUnitSymTable = record
                           UnitName: string;

                           TblLen: Integer;
                           SymTbl: array of TSymbol;
                     end;

var Units: array of TUnitSymTable = nil;

function GetUnitSymTable(const UnitIdent: string): PUnitSymTable;

// ----------------------------------------------------------------------------

implementation
uses SysUtils, Windows;

const DELTA_ALLOC = 16;

// ----------------------------------------------------------------------------

var FindSymbol_I, FindSymbol_J: Integer;

function FindSymbol(const Ident: string; const InUnit: string = ''): TSymbol;
// Plain linear search
var I, J: Integer;
begin
     for I:=0 to Length(Units)-1 do
       with Units[I] do
         if (InUnit = '') or (CompareText(UnitName, InUnit) = 0) then
         begin
            for J := 0 to TblLen-1 do
                if CompareText(Ident, SymTbl[J].Ident) = 0 then
                begin
                     //OutputDebugString(PChar(Format('FindSymbol: "%s.%s" => "%s.%s @ [%d/%d;%d/%d]"', [inunit,ident,unitname,SymTbl[J].Ident,I,Integer(Length(Units)-1),J,TblLen-1])));
                     FindSymbol_I := I;
                     FindSymbol_J := J;
                     Result := SymTbl[J];
                     Exit;
                end;

            if InUnit <> '' then Break;
       end;

     Result := nil;
     Exit;
end;

procedure AddSymbol(const Symbol: TSymbol; const ToUnit: string = '');
// Plain linear search unit + add at the end
var DestUnit: string;
    I, Len: Integer;
    OldSym: TSymbol;
begin
     Assert(Symbol<>nil, 'AddSymbol(nil)');

     if ToUnit = '' then DestUnit := CurrentUnit
                    else DestUnit := ToUnit;

     //OutputDebugString(PChar(Format('AddSymbol("%s", "%s")', [Symbol.Ident, ToUnit])));
     if Assigned(OnProgress) then OnProgress(DestUnit, Symbol.Ident); 

     if AllImported then Symbol.Imported := True;

     OldSym := FindSymbol(Symbol.Ident, DestUnit);
     if (OldSym <> nil) and (OldSym.ForwardDecl) then
     begin
          //OutputDebugString(PChar(Format('AddSymbol("%s", "%s"): forward declararation overwritten', [Symbol.Ident, ToUnit])));
          OldSym.ForwardedTo := Symbol;
          //OldSym.Free;
          Units[FindSymbol_I].SymTbl[FindSymbol_J] := Symbol;
          Exit;
     end;

     Len := Length(Units);
     for I:=0 to Len-1 do
       with Units[I] do
         if CompareText(UnitName, DestUnit) = 0 then
           begin
                Assert(TblLen <= Length(SymTbl));
                if TblLen = Length(SymTbl) then
                   SetLength(SymTbl, TblLen + DELTA_ALLOC);
                SymTbl[TblLen] := Symbol;
                Inc(TblLen);

                //OutputDebugString(PChar(Format('  ... added to [%d;%d]', [I,TblLen-1])));
                Exit;
           end;

     SetLength(Units, Len+1);
     //OutputDebugString(pchar(format('setlength(units) => %d', [len+1])));
     with Units[Len] do
     begin
          UnitName := DestUnit;
          TblLen := 1;
          SetLength(SymTbl, DELTA_ALLOC);
          SymTbl[0] := Symbol;
     end;
end;

function GetUnitSymTable(const UnitIdent: string): PUnitSymTable;
// Plain linear search
var I: Integer;
begin
     for I:=0 to Length(Units)-1 do
       with Units[I] do
         if (CompareText(UnitName, UnitIdent) = 0) then
         begin
              Result := @Units[I];
              Exit;
         end;

     Result := nil;
     Exit;
end;

// ----------------------------------------------------------------------------

procedure AddBuiltinSymbols;
var typBoolean: TType;
    typInteger: TType;
    typLongint: TType;
begin
     // ************* Built-in TYPES ***************

     // Boolean types
     typBoolean := TType.CreateEx('Boolean', 'Boolean', 'Boolean', True);
     AddSymbol(typBoolean, 'System');
     AddSymbol(TType.CreateEx('ByteBool', 'ByteBool', 'ByteBool', True), 'System');
     AddSymbol(TType.CreateEx('WordBool', 'WordBool', 'WordBool', True), 'System');
     AddSymbol(TType.CreateEx('LongBool', 'LongBool', 'LongBool', True), 'System');

     // Character types
     AddSymbol(TType.CreateEx('Char', 'Char', 'Char', True), 'System');
     AddSymbol(TType.CreateEx('AnsiChar', 'AnsiChar', 'AnsiChar', True), 'System');
     AddSymbol(TType.CreateEx('WideChar', 'WideChar', 'WideChar', True), 'System');

     // Integer types
     typInteger := TType.CreateEx('Integer', 'Integer', 'Integer', True);
     AddSymbol(typInteger, 'System');
     AddSymbol(TType.CreateEx('Cardinal', 'Cardinal', 'Cardinal', True), 'System');
     AddSymbol(TType.CreateEx('Shortint', 'Shortint', 'Shortint', True), 'System');
     AddSymbol(TType.CreateEx('Smallint', 'Smallint', 'Smallint', True), 'System');
     typLongint := TType.CreateEx('Longint', 'Longint', 'Longint', True);
     AddSymbol(typLongint, 'System');
     AddSymbol(TType.CreateEx('Int64', 'Int64', 'Int64', True), 'System');
     AddSymbol(TType.CreateEx('Byte', 'Byte', 'Byte', True), 'System');
     AddSymbol(TType.CreateEx('Word', 'Word', 'Word', True), 'System');
     AddSymbol(TType.CreateEx('LongWord', 'LongWord', 'LongWord', True), 'System');

     // Real types
     AddSymbol(TType.CreateEx('Real', 'Real', 'Real', True), 'System');
     AddSymbol(TType.CreateEx('Real48', 'Real48', 'Real48', True), 'System');
     AddSymbol(TType.CreateEx('Single', 'Single', 'Single', True), 'System');
     AddSymbol(TType.CreateEx('Double', 'Double', 'Double', True), 'System');
     AddSymbol(TType.CreateEx('Extended', 'Extended', 'Extended', True), 'System');
     AddSymbol(TType.CreateEx('Comp', 'Comp', 'Comp', True), 'System');
     AddSymbol(TType.CreateEx('Currency', 'Currency', 'Currency', True), 'System');

     // String types
     AddSymbol(TType.CreateEx('ShortString', 'ShortString', 'ShortString', True), 'System');
     AddSymbol(TType.CreateEx('AnsiString', 'AnsiString', 'AnsiString', True), 'System');
     AddSymbol(TType.CreateEx('WideString', 'WideString', 'WideString', True), 'System');

     // Pointer types
     AddSymbol(TType.CreateEx('Pointer', 'Pointer', 'Pointer', True), 'System');

     AddSymbol(TType.CreateEx('PChar', 'PChar', 'PChar', True), 'System');
     AddSymbol(TType.CreateEx('PAnsiChar', 'PAnsiChar', 'PAnsiChar', True), 'System');
     AddSymbol(TType.CreateEx('PWideChar', 'PWideChar', 'PWideChar', True), 'System');

     // File types
     AddSymbol(TType.CreateEx('Text', 'Text', 'Text', True), 'System');
     AddSymbol(TType.CreateEx('TextFile', 'TextFile', 'TextFile', True), 'System');

     // Variant types
     AddSymbol(TType.CreateEx('Variant', 'Variant', 'Variant', True), 'System');
     AddSymbol(TType.CreateEx('OleVariant', 'OleVariant', 'OleVariant', True), 'System');

     // ************* Built-in CONSTANTS ***************

     AddSymbol(TConst.CreateEx('True', 'True', {typBoolean, }True), 'System');
     AddSymbol(TConst.CreateEx('False', 'False', {typBoolean, }True), 'System');

     AddSymbol(TConst.CreateEx('MaxInt', 'MaxInt', {typInteger, }True), 'System');
     AddSymbol(TConst.CreateEx('MaxLongint', 'MaxLongint', {typLongint, }True), 'System');
end;

// ----------------------------------------------------------------------------

{ TSymbol }

constructor TSymbol.CreateEx(const I, D: string; const Imp: Boolean);
begin
     Ident := I;
     Decl := D;
     Imported := Imp;
end;

function TSymbol.GetForwardedTo: TSymbol;
begin
     Result := Self;
     while Result.ForwardDecl do
     begin
          Assert(Result <> nil);
          Result := Result.ForwardedTo;
     end;
end;

{ TType }

constructor TType.CreateEx(const I, D, N: string; const Imp: Boolean);
begin
     inherited CreateEx(I, D, Imp);
     Name := N;
end;

// ----------------------------------------------------------------------------

initialization
   AddBuiltinSymbols;
end.
