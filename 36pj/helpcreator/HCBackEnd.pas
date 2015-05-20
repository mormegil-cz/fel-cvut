(******************************************************************************
 *                                                                            *
 *    Delphi Component Help Creator                                           *
 *    version 0.1                                                             *
 *                                                                            *
 *    Semestralni prace z predmetu PJ                                         *
 *    Petr Kadlec <kadlecp2@fel.cvut.cz>                                      *
 *                                                                            *
 ******************************************************************************)

{ Back end - RTF+HPJ text generator } 
unit HCBackEnd;

interface

const CopyrightText = 'Delphi Component Help Creator v0.1 Copyright (C) DragonSoft, 2001';

procedure CreateRTF(const DstRTF_FN, UnitIdent: string);
procedure CreateHPJ(const DstHPJ_FN, DstRTF_FN, UnitName: string);

type
  TCharset = (charsetANSI, charsetMAC, charsetPC437, charsetPC850);

var Charset: TCharset = charsetANSI;

implementation
uses Windows, Classes, SysUtils, HCLexAnalyser, HCSymTable, HCEngine;

var UnitName: string;

const maskVis   = $00000007;
      maskRead  = $00000010;
      maskWrite = $00000020;
      maskRW    = maskRead or maskWrite;

      liProtected = $00000001;
      liPublished = $00000002;
      liAutomated = $00000004;

      liReadOnly  = $00000008;
      liWriteOnly = $00000010;
      liNoAccess  = $00000020;

      liVisMask   = liProtected or liPublished or liAutomated;
      liAccMask   = liReadOnly or liWriteOnly or liNoAccess; 

var UsedLegendTypes: TStringList = nil;

function FormatDeclaration(const Decl: string): string;
var I, Len, State: Integer;
    C: Char;
    Word: string;
begin
     I := 1;
     Len := Length(Decl);
     State := 0;
     Result := '';
     while I<=Len do
     begin
          C := Decl[I];
          Inc(I);
          case State of
               0: case C of
                       'A'..'Z',
                       'a'..'z',
                       '0'..'9',
                       '_': begin
                                 Word := C;
                                 State := 1;
                            end;
                       '''': begin
                                  Result := Result + C;
                                  State := 3;
                             end;
                  end;
               1: case C of
                       'A'..'Z',
                       'a'..'z',
                       '0'..'9',
                       '_': Word := Word + C;
                       else begin
                                 if IsBoldWord(Word) then
                                 begin
                                      Result := Result + '\b ' + Word + '\b0 ';
                                 end else
                                 begin
                                      //if FindSymbol(Word)<>nil then
                                      //   Result := Result + '{\uldb ' + Word + '}{\v !AL(`' + Word + ''',1)}'
                                      //else
                                         Result := Result + Word;
                                 end;
                                 case C of
                                      ' ':  State := 2;
                                      '''': begin
                                                 Result := Result + ' ';
                                                 State := 3;
                                            end;
                                      else  State := 0;
                                 end;
                            end;
                  end;
               2: case C of
                       'A'..'Z',
                       'a'..'z',
                       '0'..'9',
                       '_': begin
                                 Result := Result + ' ';
                                 Word := C;
                                 State := 1;
                            end;
                       '''': begin
                                  Result := Result + ' ''';
                                  State := 3;
                             end;
                       ' ': ;
                       else begin
                                 //Result := Result + C;
                                 State := 0;
                            end;
                  end;
               3: case C of
                       '{', '}', '\': Result := Result + '\' + C;
                       '''':          State := 0;
                       else           Result := Result + C;
                  end;
               else Assert(False);
          end;

          if State=0 then
             case C of
                  // dot/double dot
                  '.': begin
                            if (I <= Len) and (Decl[I] = '.') then Result := Result + ' ';
                            Result := Result + C;
                            if (I > 2) and (Decl[I-2] = '.') then Result := Result + ' ';
                       end;
                  // space on both sides
                  '-', '+', '*', '/',
                  '=': Result := Result + ' ' + C+ ' ';
                  // space before
                  '[': Result := Result + ' ' + C;
                  // space behind
                  ';', ']', ',',
                  ':': Result := Result + C + ' ';
                  // special characters
                  '{', '}', '\': Result := Result + '\' + C;
                  // space
                  ' ': {ignore};
                  // the rest
                  else Result := Result + C;
             end;
     end;
     if State=1 then
        if IsBoldWord(Word) then
        begin
             Result := Result + '\b ' + Word + '\b0 ';
        end else
        begin
             //if FindSymbol(Word)<>nil then
             //   Result := Result + '{\uldb ' + Word + '}{\v !AL(`' + Word + ''',1)}'
             //else
                Result := Result + Word;
        end;
end;

procedure OutputRecord(var F: TextFile; const R: TRecordType; Indent: Integer);
var I: Integer;
begin
     Assert(R<>nil);

     for I:=0 to Length(R.Fields)-1 do
       with R.Fields[I] do
           Write(F, '\par ', '':(4+2*Indent), Ident, ': ', FormatDeclaration(Typ.GetForwardedTo.Decl),';');

     if R is TVarRecordType then
       with TVarRecordType(R) do
       begin
            Write(F, '\par ', '':(4+2*Indent), '\b case\plain\f1\fs20\cf2  ');
            if SwitchVarIdent <> '' then
               Write(F, SwitchVarIdent, ': ');
            Writeln(F, FormatDeclaration(SwitchVarType.GetForwardedTo.Decl), ' \b of\plain\f1\fs20\cf2');

            Inc(Indent);
            for I:=0 to Length(VariantParts)-1 do
                with VariantParts[I] do
                begin
                     Write(F, '\par ', '':(4+2*Indent), FormatDeclaration(SwitchVarValues),': (');
                     if Fields <> nil then
                     begin
                          Writeln(F);
                          OutputRecord(F, Fields, Indent+1);
                          Writeln(F, '\par ', '':(4+2*Indent), ');');
                     end else Writeln(F, ');');
                end;
       end;
end;

procedure OutputHierarchy(var F: TextFile; const Ident: string; const Current: Boolean = True);
var S: TSymbol;
    P: string;
begin
     S := FindSymbol(Ident);
     if S = nil then Exit;

     if S is TClassType then
     begin
          if (TObjectiveType(S).SuperClass='') and (TObjectiveType(S).Kind = okClass) then
             if CompareText(Ident, 'TObject') <> 0 then P := 'TObject'
                                                   else P := ''
          else
             P := TObjectiveType(S).SuperClass;
     end else
     begin
          Assert(S is TInterfaceType);

          if TObjectiveType(S).SuperClass='' then
             if CompareText(Ident, 'IUnknown') <> 0 then P := 'IUnknown'
                                                    else P := ''
          else
             P := TObjectiveType(S).SuperClass;
     end;

     if P <> '' then
     begin
          OutputHierarchy(F, P, False);
     end;
     if not Current then
     begin
          if P <> '' then Writeln(F, '\par\tab\tab\{bmc HIERLINE.BMP\}');
          Writeln(F, '\par\uldb ', Ident, '\plain\fs20 {\v !AL(', Ident,'_object,1)}');
     end;
end;

{ **************************************************************************** }

procedure WriteVariable(var RTF: TextFile; const S: TVariable);
begin
     Assert(S<>nil);
     // Title, contents, identifier footnotes
     Writeln(RTF, '\pard\plain');
     Writeln(RTF, '{\up $}{\footnote\pard\plain{\up $}', S.Ident, ' variable}');
     Writeln(RTF, '{\up K}{\footnote\pard\plain{\up K}', S.Ident, ' variable}');
     Writeln(RTF, '{\up #}{\footnote\pard\plain{\up #}', S.Ident, '}');
     Writeln(RTF, '{\up A}{\footnote\pard\plain{\up A}', S.Ident, '_variable;', S.Ident, '}');
     // Title
     Writeln(RTF, '\pard\keepn\sb35\sa55\li65\ri65\b\fs24 ', S.Ident, ' variable');
     // par \pard\keepn\sa15\li65\ri65\tx1435\tx2695\tx3955\tx5225\tx6485 \uldb \plain\uldb\fs16\cf2 See also\plain\fs16\cf2 {\v !AL(`High_function;Integer_type')}
     // One-line short description
     Writeln(RTF, '\par\pard\sb35\sa55\li65\ri65\tx1435\tx2695\tx3955\tx5225\tx6485\plain\fs20 <short info>');
     // Unit
     Writeln(RTF, '\par\pard\sb135\li65\ri65\tx1435\tx2695\tx3955\tx5225\tx6485\b Unit');
     // Writeln(RTF, '\par\pard\sb35\sa55\li65\ri65\tx1435\tx2695\tx3955\tx5225\tx6485\uldb\plain\uldb\fs20 ', UnitName,'\plain\fs20 {\v ', LastUnit,'_routine>pme}');
     Writeln(RTF, '\par\pard\sb35\sa55\li65\ri65\tx1435\tx2695\tx3955\tx5225\tx6485\plain\fs20 ', UnitName);
     // Declaration
     Writeln(RTF, '\par\pard\sb35\li285\ri65\fi-215\tx285\tx575\tx855\tx1145\tx1435\f1 ', FormatDeclaration(S.Decl));
     // Description
     Writeln(RTF, '\par\pard\sb135\li65\ri65\tx285\tx575\tx855\tx1145\tx1435\plain\b\fs20 Description');
     Writeln(RTF, '\par\pard\sb35\sa55\li65\ri65\tx285\tx575\tx855\tx1145\tx1435\plain\fs20 <description>');

     Writeln(RTF, '\par\page');
end;

procedure WriteRecord(var RTF: TextFile; const S: TRecordType);
begin
     Assert(S<>nil);
     // Title, contents, identifier footnotes
     Writeln(RTF, '\pard\plain');
     Writeln(RTF, '{\up $}{\footnote\pard\plain{\up $}', S.Ident, ' type}');
     Writeln(RTF, '{\up K}{\footnote\pard\plain{\up K}', S.Ident, ' type}');
     Writeln(RTF, '{\up #}{\footnote\pard\plain{\up #}', S.Ident, '}');
     Writeln(RTF, '{\up A}{\footnote\pard\plain{\up A}', S.Ident, '_type;', S.Ident, '}');
     // Title
     Writeln(RTF, '\pard\keepn\sb35\sa55\li65\ri65\b\fs24 ', S.Ident, ' type');
     // par \pard\keepn\sa15\li65\ri65\tx1435\tx2695\tx3955\tx5225\tx6485 \uldb \plain\uldb\fs16\cf2 See also\plain\fs16\cf2 {\v !AL(`High_function;Integer_type')}
     // One-line short description
     Writeln(RTF, '\par\pard\sb35\sa55\li65\ri65\tx1435\tx2695\tx3955\tx5225\tx6485\plain\fs20 <short info>');
     // Unit
     Writeln(RTF, '\par\pard\sb135\li65\ri65\tx1435\tx2695\tx3955\tx5225\tx6485\b Unit');
     // Writeln(RTF, '\par\pard\sb35\sa55\li65\ri65\tx1435\tx2695\tx3955\tx5225\tx6485\uldb\plain\uldb\fs20 ', UnitName,'\plain\fs20 {\v ', UnitName,'_routine>pme}');
     Writeln(RTF, '\par\pard\sb35\sa55\li65\ri65\tx1435\tx2695\tx3955\tx5225\tx6485\plain\fs20 ', UnitName);

     // Declaration
     Writeln(RTF, '\par\pard\sb35\li285\ri65\fi-215\tx285\tx575\tx855\tx1145\tx1435\f1\b type\b0\line');
     Write(RTF, '\plain\f1\fs20\cf2 ', S.Ident, ' = \b ');
     if TRecordType(S).IsPacked then Write(RTF, 'packed ');
     Writeln(RTF, 'record\b0\plain\f1\fs20\cf2');
     Writeln(RTF, '\pard\li285\ri65\fi-215\tx285\tx575\tx855\tx1145\tx1435');

     OutputRecord(RTF, TRecordType(S), 0);

     Writeln(RTF, '\par\pard\sb35\li285\ri65\fi-215\tx285\tx575\tx855\tx1145\tx1435\b   end\plain\f1\fs20\cf2 ;');

     // Description
     Writeln(RTF, '\par\pard\sb135\li65\ri65\tx285\tx575\tx855\tx1145\tx1435\plain\b\fs20 Description');
     Writeln(RTF, '\par\pard\sb35\sa55\li65\ri65\tx285\tx575\tx855\tx1145\tx1435\plain\fs20 <description>');

     Writeln(RTF, '\par\page');
end;

procedure WriteType(var RTF: TextFile; const S: TType);
begin
     Assert(S<>nil);
     // Title, contents, identifier footnotes
     Writeln(RTF, '\pard\plain');
     Writeln(RTF, '{\up $}{\footnote\pard\plain{\up $}', S.Ident, ' type}');
     Writeln(RTF, '{\up K}{\footnote\pard\plain{\up K}', S.Ident, ' type}');
     Writeln(RTF, '{\up #}{\footnote\pard\plain{\up #}', S.Ident, '}');
     Writeln(RTF, '{\up A}{\footnote\pard\plain{\up A}', S.Ident, '_type;', S.Ident, '}');
     // Title
     Writeln(RTF, '\pard\keepn\sb35\sa55\li65\ri65\b\fs24 ', S.Ident, ' type');
     // par \pard\keepn\sa15\li65\ri65\tx1435\tx2695\tx3955\tx5225\tx6485 \uldb \plain\uldb\fs16\cf2 See also\plain\fs16\cf2 {\v !AL(`High_function;Integer_type')}
     // One-line short description
     Writeln(RTF, '\par\pard\sb35\sa55\li65\ri65\tx1435\tx2695\tx3955\tx5225\tx6485\plain\fs20 <short info>');
     // Unit
     Writeln(RTF, '\par\pard\sb135\li65\ri65\tx1435\tx2695\tx3955\tx5225\tx6485\b Unit');
     // Writeln(RTF, '\par\pard\sb35\sa55\li65\ri65\tx1435\tx2695\tx3955\tx5225\tx6485\uldb\plain\uldb\fs20 ', UnitName,'\plain\fs20 {\v ', UnitName,'_routine>pme}');
     Writeln(RTF, '\par\pard\sb35\sa55\li65\ri65\tx1435\tx2695\tx3955\tx5225\tx6485\plain\fs20 ', UnitName);
     // Declaration
     Writeln(RTF, '\par\pard\sb35\li285\ri65\fi-215\tx285\tx575\tx855\tx1145\tx1435\f1 ', FormatDeclaration('type '+S.Ident+'='+S.Decl+';'));
     // Description
     Writeln(RTF, '\par\pard\sb135\li65\ri65\tx285\tx575\tx855\tx1145\tx1435\plain\b\fs20 Description');
     Writeln(RTF, '\par\pard\sb35\sa55\li65\ri65\tx285\tx575\tx855\tx1145\tx1435\plain\fs20 <description>');

     Writeln(RTF, '\par\page');
end;

procedure WriteConst(var RTF: TextFile; const S: TConst);
begin
     Assert(S<>nil);
     // Title, contents, identifier footnotes
     Writeln(RTF, '\pard\plain');
     Writeln(RTF, '{\up $}{\footnote\pard\plain{\up $}', S.Ident, ' constant}');
     Writeln(RTF, '{\up K}{\footnote\pard\plain{\up K}', S.Ident, ' constant}');
     Writeln(RTF, '{\up #}{\footnote\pard\plain{\up #}', S.Ident, '}');
     Writeln(RTF, '{\up A}{\footnote\pard\plain{\up A}', S.Ident, '_constant;', S.Ident, '}');
     // Title
     Writeln(RTF, '\pard\keepn\sb35\sa55\li65\ri65\b\fs24 ', S.Ident, ' constant');
     //\par \pard\keepn\sa15\li65\ri65\tx1435\tx2695\tx3955\tx5225\tx6485 \uldb \plain\uldb\fs16\cf2 See also\plain\fs16\cf2 {\v !AL(`High_function;Integer_type')}
     // One-line short description
     Writeln(RTF, '\par\pard\sb35\sa55\li65\ri65\tx1435\tx2695\tx3955\tx5225\tx6485\plain\fs20 <short info>');
     // Unit
     Writeln(RTF, '\par\pard\sb135\li65\ri65\tx1435\tx2695\tx3955\tx5225\tx6485\b Unit');
     //Writeln(RTF, '\par\pard\sb35\sa55\li65\ri65\tx1435\tx2695\tx3955\tx5225\tx6485\uldb\plain\uldb\fs20 ', UnitName,'\plain\fs20 {\v ', LastUnit,'_routine>pme}');
     Writeln(RTF, '\par\pard\sb35\sa55\li65\ri65\tx1435\tx2695\tx3955\tx5225\tx6485\plain\fs20 ', UnitName);
     // Declaration
     Writeln(RTF, '\par\pard\sb35\li285\ri65\fi-215\tx285\tx575\tx855\tx1145\tx1435\f1 ', FormatDeclaration(S.Decl));
     // Description
     Writeln(RTF, '\par\pard\sb135\li65\ri65\tx285\tx575\tx855\tx1145\tx1435\plain\b\fs20 Description');
     Writeln(RTF, '\par\pard\sb35\sa55\li65\ri65\tx285\tx575\tx855\tx1145\tx1435\plain\fs20 <description>');

     Writeln(RTF, '\par\page');
end;

procedure WriteProc(var RTF: TextFile; const S: TProc);
const ProcType: array[TProcKind] of string = ('function', 'procedure');
begin
     Assert(S<>nil);
     Writeln(RTF, '\pard\plain');

     // Title, contents, identifier footnotes
     Writeln(RTF, '{\up $}{\footnote\pard\plain{\up $}', S.Ident, ' ', ProcType[TProc(S).ProcKind],'}');
     Writeln(RTF, '{\up K}{\footnote\pard\plain{\up K}', S.Ident, ' ', ProcType[TProc(S).ProcKind],'}');
     Writeln(RTF, '{\up #}{\footnote\pard\plain{\up #}', S.Ident, '}');
     Writeln(RTF, '{\up #}{\footnote\pard\plain{\up #}', S.Ident, '_', ProcType[TProc(S).ProcKind], ';', S.Ident, '}');
     // Title
     Writeln(RTF, '\pard\keepn\sb32\sa55\li65\ri65\b\fs24 ', S.Ident, ' ', ProcType[TProc(S).ProcKind]);
     // One-line short description
     Writeln(RTF, '\par\pard\sb35\sa55\li65\ri65\tx1435\tx2695\tx3955\tx5225\tx6485\plain\fs20 <short info>');
     // Unit
     Writeln(RTF, '\par\pard\sb135\li65\ri65\tx1435\tx2695\tx3955\tx5225\tx6485\b Unit');
     //Writeln(RTF, '\par\pard\sb35\sa55\li65\ri65\tx1435\tx2695\tx3955\tx5225\tx6485\uldb\plain\uldb\fs20 ', UnitName,'\plain\fs20 {\v ', LastUnit,'_routine>pme}');
     Writeln(RTF, '\par\pard\sb35\sa55\li65\ri65\tx1435\tx2695\tx3955\tx5225\tx6485\plain\fs20 ', UnitName);
     (*
     // Category
     \par \pard\sb135\li65\ri65\tx1435\tx2695\tx3955\tx5225\tx6485 \b Category
     \par \pard\sb35\sa55\li65\ri65\tx1435\tx2695\tx3955\tx5225\tx6485 \uldb \plain\uldb\fs20 exception handling routines\plain\fs20 {\v Exception handling routines_category>pme}
     *)
     // Declaration
     Writeln(RTF, '\par\pard\sb35\li285\ri65\fi-215\tx285\tx575\tx855\tx1145\tx1435\f1 ', FormatDeclaration(S.Decl));
     // Description
     Writeln(RTF, '\par\pard\sb135\li65\ri65\tx285\tx575\tx855\tx1145\tx1435\plain\b\fs20 Description');
     Writeln(RTF, '\par\pard\sb35\sa55\li65\ri65\tx285\tx575\tx855\tx1145\tx1435\plain\fs20 <description>');

     Writeln(RTF, '\par\page');
end;

procedure WriteObjMainPage(var RTF: TextFile; const S: TObjectiveType;
                           const HasVariables, HasProperties, HasMethods, HasEvents: Boolean);
begin
     Assert(S<>nil);
     // Title, contents, identifier footnotes
     Writeln(RTF, '\pard\plain');
     Writeln(RTF, '{\up $}{\footnote\pard\plain{\up $}', S.Ident, '}');
     Writeln(RTF, '{\up K}{\footnote\pard\plain{\up K}', S.Ident, ',}');
     Writeln(RTF, '{\up #}{\footnote\pard\plain{\up #}', S.Ident, '}');
     Write(RTF, '{\up A}{\footnote\pard\plain{\up A}', S.Ident, '_object;', S.Ident);
     if S is TInterfaceType then Write(RTF, ';', S.Ident, '_interface');
     Writeln(RTF, '}');
     // Title
     Writeln(RTF, '\pard\keepn\sb35\sa55\li65\ri65\b\fs24 ', S.Ident);
     // Hierarchy, properties, methods, events
     Writeln(RTF, '\par\pard\keepn\sa15\li65\ri65\tx1435\tx2695\tx3955\tx5225\tx6485');
     Write(RTF, '\plain\fs16\cf1');
     if (S.Kind in [okClass, okInterface]) or ((S.Kind = okObject) and (S.SuperClass <> '')) then
        Writeln(RTF, '\ul Hierarchy\plain\fs16\cf1 {\v ', S.Ident ,'_hierarchy}\tab');
     if HasVariables then
        Writeln(RTF, '\uldb Variables\plain\fs16\cf1 {\v ', S.Ident, '_variables>pme}\tab');
     if HasProperties then
        Writeln(RTF, '\uldb Properties\plain\fs16\cf1 {\v ', S.Ident, '_properties>pme}\tab');
     if HasMethods then
        Writeln(RTF, '\uldb Methods\plain\fs16\cf1 {\v ', S.Ident, '_methods>pme}\tab');
     if HasEvents then
        Writeln(RTF, '\uldb Events\plain\fs16\cf1 {\v ', S.Ident, '_events>pme}\tab');
     // One-line short description
     Writeln(RTF, '\par\pard\sb35\sa55\li65\ri65\tx1435\tx2695\tx3955\tx5225\tx6485\plain\fs20 <short info>');
     // Unit
     Writeln(RTF, '\par\pard\sb135\li65\ri65\tx1435\tx2695\tx3955\tx5225\tx6485\b Unit');
     // Writeln(RTF, '\par\pard\sb35\sa55\li65\ri65\tx1435\tx2695\tx3955\tx5225\tx6485\uldb\plain\uldb\fs20 ', UnitName,'\plain\fs20 {\v ', UnitName,'_routine>pme}');
     Writeln(RTF, '\par\pard\sb35\sa55\li65\ri65\tx1435\tx2695\tx3955\tx5225\tx6485\plain\fs20 ', UnitName);
     // Description
     Writeln(RTF, '\par\pard\sb135\li65\ri65\tx285\tx575\tx855\tx1145\tx1435\plain\b\fs20 Description');
     Writeln(RTF, '\par\pard\sb35\sa55\li65\ri65\tx285\tx575\tx855\tx1145\tx1435\plain\fs20 <description>');

     Writeln(RTF, '\par\page');
end;

procedure WriteObjHierarchy(var RTF: TextFile; const S: TObjectiveType);
begin
     Assert(S<>nil);

     Writeln(RTF, '{\up #}{\footnote\pard\plain{\up #} ',S.Ident ,'_hierarchy}');
     Writeln(RTF, '\pard\sb135\li65\ri65 \b Hierarchy');
     Writeln(RTF, '\par\pard\sa55\li285\ri65\fi-215\tx185\tx285\plain\fs20');

     OutputHierarchy(RTF, S.Ident);

     Writeln(RTF, '\par');
     Writeln(RTF, '\par');
     Writeln(RTF, '\par\page');
end;

function IsProcType(T: TType): Boolean;
begin
     if T = nil then
     begin
          Result := False;
          Exit;
     end;
     T := T.GetForwardedTo as TType;
     while T is TEqualityType do
           T := TEqualityType(T).EqualTo.GetForwardedTo as TType;

     Result := T is TProcType;
end;

procedure WriteList(var RTF: TextFile; const S: TObjectiveType; const List: TStringList;
                    const Kind, LegendTopic: string);
var I: Integer;
    Flags: Cardinal;
begin
     Assert((S<>nil) and (List<>nil));

     List.Sort;

     Writeln(RTF, '{\up #}{\footnote\pard\plain{\up #}', S.Name, '_', Kind,'}');
     Writeln(RTF, '\pard\keepn\sb35\sa55\li65\ri65\tx175\b\fs24 ', S.Name, ' ', Kind);
     Writeln(RTF, '\par\pard\keepn\sa15\li65\ri65\tx175\tx1435\tx2695\tx3955\tx5225\tx6485');
     Writeln(RTF, '\plain\uldb\fs16\cf1 ', S.Name, '\plain\fs16\cf1{\v ', S.Name, '>main}');
     if LegendTopic <> 'legend-' then
        Writeln(RTF, '\tab\ul Legend\plain\fs16\cf1 {\v ', LegendTopic, '}');

     Writeln(RTF, '\par\pard\sa55\li285\ri65\fi-215\tx175\tx285\plain\fs20');
     for I:=0 to List.Count-1 do
     begin
          if I = 0 then Write(RTF, '\par\pard\sa55\li285\ri65\fi-215\tx175\tx285\plain\fs20')
                   else Write(RTF, '\par');

          Flags := Cardinal(List.Objects[I]);
          //OutputDebugString(PChar(Format('WriteList "%s.%s" Flags=%x', [S.Name, List[I], Flags])));
          case Flags and liVisMask of
               0:           ;
               liProtected: Write(RTF, '\{bmc VIS_PROT.BMP\}');
               liPublished: Write(RTF, '\{bmc VIS_PBSH.BMP\}');
               liAutomated: Write(RTF, '\{bmc VIS_AUTO.BMP\}');
               else         Assert(False);
          end;
          Write(RTF, '\tab');
          case Flags and liAccMask of
               0:           ;
               liReadOnly:  Write(RTF, '\{bmc ACC_RO.BMP\}');
               liWriteOnly: Write(RTF, '\{bmc ACC_WO.BMP\}');
               liNoAccess:  Write(RTF, '\{bmc ACC_NONE.BMP\}');
               else         Assert(False);
          end;
          Writeln(RTF, '\tab\uldb ', List[I], '\plain\fs20{\v ', S.Name, '_', List[I], '>main}');
     end;

     Writeln(RTF, '\par\page');
end;

function LegendTopic(LegendItems: Cardinal): string;
begin
     Result := 'legend-';
     if LegendItems = 0 then Exit;

     if (LegendItems and liProtected) <> 0 then Result := Result + 'P';
     if (LegendItems and liPublished) <> 0 then Result := Result + 'U';
     if (LegendItems and liAutomated) <> 0 then Result := Result + 'A';
     if (LegendItems and liNoAccess)  <> 0 then Result := Result + 'N';
     if (LegendItems and liReadOnly)  <> 0 then Result := Result + 'R';
     if (LegendItems and liWriteOnly) <> 0 then Result := Result + 'W';
     UsedLegendTypes.AddObject(Result, TObject(LegendItems));

     //OutputDebugString(PChar(Format('LegendTopic(%x) = "%s"', [LegendItems, Result])));
end;

procedure WriteObjMember(var RTF: TextFile; const S: TObjectiveType; const M: TObjMember; const Kind: string);
begin
     Assert((S<>nil) and (M<>nil));

     // Title, contents, identifier footnotes
     Writeln(RTF, '\pard\plain');
     Writeln(RTF, '{\up $}{\footnote\pard\plain{\up $}', S.Name, '.', M.Name, '}');
     Writeln(RTF, '{\up #}{\footnote\pard\plain{\up #}', S.Name, '_', M.Name, '}');
     Writeln(RTF, '{\up A}{\footnote\pard\plain{\up A}', S.Name, ';', S.Name, '_', Kind, ';', S.Name, '_', M.Name, '}');
     Writeln(RTF, '{\up K}{\footnote\pard\plain{\up K}', M.Name, ',;', M.Name, ',', S.Name, ';', S.Name, ',;', S.Name, ',', M.Name, '}');
     // Title
     Writeln(RTF, '\pard\keepn\sb35\sa55\li65\ri65\b\fs24 ', S.Name, '.', M.Name);
     // First line
     Writeln(RTF, '\par\pard\keepn\sa15\li65\ri65\tx1435\tx2695\tx3955\tx5225\tx6485\uldb\plain\uldb\fs16\cf1 ',
                  S.Name,'\plain\fs16\cf1{\v ', S.Name, '>main}');
     // One-line short description
     Writeln(RTF, '\par\pard\sb35\sa55\li65\ri65\tx1435\tx2695\tx3955\tx5225\tx6485\plain\fs20 <short info>');
     // Declaration
     Writeln(RTF, '\par\pard\sb35\li285\ri65\fi-215\tx285\tx575\tx855\tx1145\tx1435\f1 ', FormatDeclaration(M.Decl));
     // Description
     Writeln(RTF, '\par\pard\sb135\li65\ri65\tx285\tx575\tx855\tx1145\tx1435\plain\b\fs20 Description');
     Writeln(RTF, '\par\pard\sb35\sa55\li65\ri65\tx285\tx575\tx855\tx1145\tx1435\plain\fs20 <description>');

     Writeln(RTF, '\par\page');
end;

function LFlags(const F: Cardinal): Cardinal;
begin
     case F and maskVis of
          Cardinal(svProtected): Result := liProtected;
          Cardinal(svPublished): Result := liPublished;
          Cardinal(svAutomated): Result := liAutomated;
          else                   Result := 0;
     end;
     case F and maskRW of
          0:         Result := Result or liNoAccess;
          maskRead:  Result := Result or liReadOnly;
          maskWrite: Result := Result or liWriteOnly;
          maskRW:    ;
          else       Assert(False);
     end;
end;

procedure WriteObject(var RTF: TextFile; const S: TObjectiveType);
var V: TObjSectionVisibility;
    Member: TObjMember;
    I: Integer;

    VarList, PropList, MetList, EventList: TStringList;
    LF, Flags: Cardinal;

    LType: array[0..3] of Cardinal;
begin
     FillChar(LType, SizeOf(LType), 0);
     VarList := nil;
     PropList := nil;
     MetList := nil;
     EventList := nil;
     try
       VarList := TStringList.Create;
       PropList := TStringList.Create;
       MetList := TStringList.Create;
       EventList := TStringList.Create;
       if S is TClassType then
       begin
          for V:=Low(V) to High(V) do
            if V <> svPrivate then // skip private section - it is not included in help
              for I:=0 to Length(TClassType(S).Sections[V])-1 do
              begin
                   Member := TClassType(S).Sections[V][I];
                   Assert(Member <> nil);
                   Flags := Cardinal(V);

                   //OutputDebugString(PChar(Format('WriteObject\%s\%s', [S.Name, Member.Name])));

                   if Member is TObjField then
                   begin
                        // object fields are not included in help
                        {
                        Flags := Flags or maskRW;
                        LF := LFlags(Flags);
                        LType[0] := LType[0] or LF;
                        VarList.AddObject(Member.Name, TObject(LF));
                        WriteObjMember(RTF, S, Member, 'variable');
                        }
                   end
                   else if Member is TObjMethod then
                   begin
                        Flags := Flags or maskRW;
                        LF := LFlags(Flags);
                        LType[1] := LType[1] or LF;
                        MetList.AddObject(Member.Name, TObject(LF));
                        WriteObjMember(RTF, S, Member, 'method');
                   end
                   else if Member is TObjProperty then
                        with TObjProperty(Member) do
                        begin
                             if CanRead  then Flags := Flags or maskRead;
                             if CanWrite then Flags := Flags or maskWrite;

                             if IsProcType(Typ) then
                             begin
                                  LF := LFlags(Flags);
                                  LType[3] := LType[3] or LF;
                                  EventList.AddObject(Member.Name, TObject(LF));
                                  WriteObjMember(RTF, S, Member, 'event');
                             end else
                             begin
                                  LF := LFlags(Flags);
                                  LType[2] := LType[2] or LF;
                                  PropList.AddObject(Member.Name, TObject(LF));
                                  WriteObjMember(RTF, S, Member, 'property');
                             end;
                        end else Assert(False);
                   //OutputDebugString(PChar(Format('WriteObject\%s\%s Flags=%x, LF=%x, LType=(%x,%x,%x,%x)', [S.Name, Member.Name,Flags,LF,Ltype[0],ltype[1],ltype[2],ltype[3]])));
              end;
       end else
       begin
            Assert(S is TInterfaceType);
            for I:=0 to Length(TInterfaceType(S).Methods)-1 do
            begin
                 Member := TInterfaceType(S).Methods[I];
                 MetList.AddObject(Member.Name, TObject(LFlags(Cardinal(svPublic) or maskRW)));
                 WriteObjMember(RTF, S, Member, 'method');
            end;
            for I:=0 to Length(TInterfaceType(S).Properties)-1 do
                with TInterfaceType(S).Properties[I] do
                begin
                     Flags := Cardinal(svPublic);
                     if CanRead  then Flags := Flags or maskRead;
                     if CanWrite then Flags := Flags or maskWrite;

                     if IsProcType(Typ) then
                     begin
                          LF := LFlags(Flags);
                          LType[3] := LType[3] or LF;
                          EventList.AddObject(Name, TObject(LF));
                          WriteObjMember(RTF, S, TInterfaceType(S).Properties[I], 'event');
                     end else
                     begin
                          LF := LFlags(Flags);
                          LType[2] := LType[2] or LF;
                          PropList.AddObject(Name, TObject(LF));
                          WriteObjMember(RTF, S, TInterfaceType(S).Properties[I], 'property');
                     end;
                end;
       end;

       //OutputDebugString(PChar(Format('LType=(%x,%x,%x,%x)', [Ltype[0],ltype[1],ltype[2],ltype[3]])));

       WriteObjMainPage(RTF, S, VarList.Count > 0, PropList.Count > 0, MetList.Count > 0, EventList.Count > 0);

       if VarList.Count > 0 then WriteList(RTF, S, VarList, 'variables', LegendTopic(LType[0]));
       if MetList.Count > 0 then WriteList(RTF, S, MetList, 'methods', LegendTopic(LType[1]));
       if PropList.Count > 0 then WriteList(RTF, S, PropList, 'properties', LegendTopic(LType[2]));
       if EventList.Count > 0 then WriteList(RTF, S, EventList, 'events', LegendTopic(LType[3]));

     finally
       EventList.Free;
       MetList.Free;
       PropList.Free;
       VarList.Free;
     end;
     if (S.Kind in [okClass, okInterface]) or ((S.Kind = okObject) and (S.SuperClass <> '')) then
        WriteObjHierarchy(RTF, S);
end;

procedure WriteLegendTopic(var RTF: TextFile; const LegendTitle: string; const LegendType: Cardinal);
begin
     if LegendType=0 then Exit;

     Writeln(RTF, '\pard\sb35\sa55\li65\ri65');
     Writeln(RTF, '\par\pard\sb135\li65\ri65{\up #}{\footnote\pard\plain{\up #}', LegendTitle,'}');

     if LegendType and (liProtected or liPublished or liAutomated) <> 0 then
     begin
          Writeln(RTF, '\b Scope');
          Writeln(RTF, '\par\pard\sa55\li285\ri65\fi-215\tx185\tx285\plain\fs20');
          if LegendType and liProtected <> 0 then
             Writeln(RTF, '\{bmc VIS_PROT.BMP\}\tab\tab Protected\par');
          if LegendType and liPublished <> 0 then
             Writeln(RTF, '\{bmc VIS_PBSH.BMP\}\tab\tab Published\par');
          if LegendType and liAutomated <> 0 then
             Writeln(RTF, '\{bmc VIS_AUTO.BMP\}\tab\tab Automated\par');
     end;
     if LegendType and (liNoAccess or liReadOnly or liWriteOnly) <> 0 then
     begin
          Writeln(RTF, '\b Accessibility');
          Writeln(RTF, '\par\pard\sa55\li285\ri65\fi-215\tx185\tx285\plain\fs20');
          if LegendType and liNoAccess <> 0 then
             Writeln(RTF, '\{bmc ACC_NONE.BMP\}\tab\tab Inherited\par');
          if LegendType and liReadOnly <> 0 then
             Writeln(RTF, '\{bmc ACC_RO.BMP\}\tab\tab Read-only\par');
          if LegendType and liWriteOnly <> 0 then
             Writeln(RTF, '\{bmc ACC_WO.BMP\}\tab\tab Write-only\par');
     end;

     Writeln(RTF, '\par\page');
end;

{ **************************************************************************** }

procedure CreateRTF(const DstRTF_FN, UnitIdent: string);
const CharsetIDs: array[TCharset] of string = ('\ansi', '\mac', '\pc', '\pca');
var RTF: TextFile;
    SymTable: PUnitSymTable;
    I: Integer;
    S: TSymbol;
begin
     UnitName := UnitIdent;
     AssignFile(RTF, DstRTF_FN);
     FileMode := 1;
     Rewrite(RTF);
     try
       UsedLegendTypes := TStringList.Create;
       UsedLegendTypes.Sorted := True;
       UsedLegendTypes.Duplicates := dupIgnore;

       Writeln(RTF, '{\rtf1', CharsetIDs[Charset], '\deff0{\fonttbl{\f0\fswiss Arial;}'+
                    '{\f1\fmodern Courier New;}'+
                    '{\f2\froman Symbol;}'+
                    '{\f3\froman Times New Roman;}}'+
                    '{\colortbl;\red0\green128\blue0;\red0\green0\blue0;\red0\green0\blue128;\red128\green0\blue0;}'+
                    '{\stylesheet{\fs24\snext0 Normal;}}');

       // about page
       Writeln(RTF, '\par\pard\sb35\sa55\li65\ri65');
       Writeln(RTF, '{\up #}{\footnote\pard\plain{\up #}About-Help-File}');
       Writeln(RTF, 'This help file is based on output of \b ', CopyrightText, '\b0');
       Writeln(RTF, '\par This program used \f1 ', UnitIdent, '.pas\plain  to automatically generate a template.');
       Writeln(RTF, '\par The source file has been parsed at ', DateTimeToStr(Now));
       Writeln(RTF, '\par\page');

       SymTable := GetUnitSymTable(UnitName);
       if SymTable = nil then
       begin
            Writeln(RTF, '}');
            Exit;
       end;

       for I:=0 to SymTable^.TblLen-1 do
       begin
            S:=SymTable^.SymTbl[I];
            if S.Imported then Continue;
            if (S.ForwardDecl) then RaiseSyntaxError(Format('''%s'' is not yet completely defined', [S.Ident]));

            if S is TObjectiveType then WriteObject(RTF, TObjectiveType(S))
            else if S is TRecordType then WriteRecord(RTF, TRecordType(S))
            else if S is TType then WriteType(RTF, TType(S))
            else if S is TVariable then WriteVariable(RTF, TVariable(S))
            else if S is TConst then
                 begin
                      if not (S is TResString) then // resource strings are not included in help
                         WriteConst(RTF, TConst(S));
                 end
            else if S is TProc then WriteProc(RTF, TProc(S))
            else Assert(False, 'Unknown TSymbol: '+S.Ident);
       end;

       for I:=0 to UsedLegendTypes.Count-1 do
           WriteLegendTopic(RTF, UsedLegendTypes[I], Cardinal(UsedLegendTypes.Objects[I]));

       Writeln(RTF, '}');
     finally
       CloseFile(RTF);
       FreeAndNil(UsedLegendTypes);
     end;
end;

procedure CreateHPJ(const DstHPJ_FN, DstRTF_FN, UnitName: string);
var HPJ: TextFile;
begin
     AssignFile(HPJ, DstHPJ_FN);
     FileMode := 1;
     Rewrite(HPJ);
     try
        Writeln(HPJ, '; This file has been automatically generated from ',UnitName, '.pas on ', DateTimeToStr(Now));
        Writeln(HPJ);
        Writeln(HPJ, '[OPTIONS]');
        Writeln(HPJ, 'TITLE=Component help for ', UnitName);
        Writeln(HPJ, 'COPYRIGHT=This help text was created using ',
                     CopyrightText,
                     ' This Help file was compiled on %date');
        Writeln(HPJ, 'LCID=0x409 0x0 0x0');
        Writeln(HPJ, 'COMPRESS=12');
        Writeln(HPJ);
        Writeln(HPJ, '[WINDOWS]');
        Writeln(HPJ, 'main="",,28676,,,f2');
        Writeln(HPJ, 'example="Example",(360,160,640,320),12292,(255,255,226),(255,255,128),f2');
        Writeln(HPJ, 'pme="",(40,320,280,640),4,(255,255,226),(192,192,192)');
        Writeln(HPJ, 'proc="",(425,100,578,300),12292,,,f2');
        Writeln(HPJ, 'gloss="",(425,100,578,300),12292,,(192,192,192),f2');
        Writeln(HPJ, '(w95sec)="",(425,100,578,300),12292,,(192,192,192),f2');
        Writeln(HPJ);
        Writeln(HPJ, '[CONFIG]');
        Writeln(HPJ, 'BrowseButtons()');
        Writeln(HPJ);
        Writeln(HPJ, '[FILES]');
        Writeln(HPJ, DstRTF_FN);
     finally
       CloseFile(HPJ);
     end;
end;

end.
