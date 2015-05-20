(******************************************************************************
 *                                                                            *
 *    Delphi Component Help Creator                                           *
 *    version 0.1                                                             *
 *                                                                            *
 *    Semestralni prace z predmetu PJ                                         *
 *    Petr Kadlec <kadlecp2@fel.cvut.cz>                                      *
 *                                                                            *
 ******************************************************************************)

{ Unit stack }
unit HCUnitStack;

interface

procedure UnitStack_Push;
procedure UnitStack_Pop;

implementation
uses Classes, Contnrs, HCInput, HCEngine, HCLexAnalyser, HCSymTable;

type TUnitState = class
                       // HCLexAnalyser
                       SourceFile: TSourceFile;
                       RawText:    string;
                       StrAttrib:  string;
                       IntAttrib:  Int64;
                       FltAttrib:  Extended;
                       RawAccum:   string;
                       // HCSymTable
                       CurrentUnit: string;
                       // HCEngine
                       UnitList: TStringList;
                       Token:    TTokenKind;
                       Symbol:   TSymbol;
                  end;

var UnitStack: TObjectStack;

procedure UnitStack_Push;
var S: TUnitState;
begin
     S := TUnitState.Create;
     with S do
     begin
          SourceFile := HCLexAnalyser.SourceFile;
          RawText    := HCLexAnalyser.RawText;
          StrAttrib  := HCLexAnalyser.StrAttrib;
          IntAttrib  := HCLexAnalyser.IntAttrib;
          FltAttrib  := HCLexAnalyser.FltAttrib;
          RawAccum   := HCLexAnalyser.RawAccum;

          CurrentUnit:= HCSymTable.CurrentUnit;

          UnitList   := HCEngine.UnitList;
          Token      := HCEngine.Token;
          Symbol     := HCEngine.Symbol;
     end;
     UnitStack.Push(S);
end;

procedure UnitStack_Pop;
var S: TUnitState;
begin
     S := UnitStack.Pop as TUnitState;
     with S do
     begin
          HCLexAnalyser.SourceFile := SourceFile;
          HCLexAnalyser.RawText    := RawText;
          HCLexAnalyser.StrAttrib  := StrAttrib;
          HCLexAnalyser.IntAttrib  := IntAttrib;
          HCLexAnalyser.FltAttrib  := FltAttrib;
          HCLexAnalyser.RawAccum   := RawAccum;

          HCSymTable.CurrentUnit   := CurrentUnit;

          HCEngine.UnitList        := UnitList;
          HCEngine.Token           := Token;
          HCEngine.Symbol          := Symbol;
     end;
     S.Free;
end;

initialization
  UnitStack := TObjectStack.Create;
finalization
  UnitStack.Free;
end.
