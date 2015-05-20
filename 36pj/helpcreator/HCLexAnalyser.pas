(******************************************************************************
 *                                                                            *
 *    Delphi Component Help Creator                                           *
 *    version 0.1                                                             *
 *                                                                            *
 *    Semestralni prace z predmetu PJ                                         *
 *    Petr Kadlec <kadlecp2@fel.cvut.cz>                                      *
 *                                                                            *
 ******************************************************************************)

{ Lexical analyser }
unit HCLexAnalyser;

interface
uses HCInput;

type
  TTokenKind = (
     lexEOF,           // end of file
     lexInvalid,       // invalid token

     lexIdent,         // identifier
     lexIntConst,      // integer constant
     lexFloatConst,    // floating-point constant
     lexStrConst,      // string constant

     lexSpecialFunc,   // special function <specialfunc>
     lexObjDirective,  // Object directive <objdirective>

     lexOtherReserved, // another reserved word
     lexOtherDirective,// another directive
     lexSYSTEM,        // System unit identifier <system>

     lexSign,          // + -
     lexRelOp,         // < > <= >= IN IS AS
     lexBinAddOp,      // OR XOR
     lexMulOp,         // * / DIV MOD AND SHL SHR

     lexSemicolon,     // ;
     lexEquals,        // =
     lexColon,         // :
     lexLeftRBracket,  // (
     lexRightRBracket, // )
     lexDotDot,        // ..
     lexLeftABracket,  // [ (.
     lexRightABracket, // ] .)
     lexComma,         // ,
     lexAccute,        // ^
     lexDot,           // .
     lexAt,            // @
     lexAmpersand,     // &  -- unused in HelpCreator
     lexAssign,        // := -- unused in HelpCreator

     // access directives <accessdirective>
     lexAUTOMATED,
     lexPUBLISHED,
     lexPRIVATE,
     lexPROTECTED,
     lexPUBLIC,

     // directives
     lexABSOLUTE,
     lexDEFAULT,
     lexDISPID,
     lexEXTERNAL,
     lexIMPLEMENTS,
     lexINDEX,
     lexMESSAGE,
     lexNAME,
     lexNODEFAULT,
     lexREAD,
     lexREADONLY,
     lexRESIDENT,
     lexSTORED,
     lexWRITE,
     lexWRITEONLY,

     // reserved words
     lexARRAY,
     lexCASE,
     lexCLASS,
     lexCONST,
     lexCONSTRUCTOR,
     lexDESTRUCTOR,
     lexDISPINTERFACE,
     lexEND,
     lexEXPORTS,
     lexFILE,
     lexFUNCTION,
     lexIMPLEMENTATION,
     lexINTERFACE,
     lexNIL,
     lexNOT,
     lexOBJECT,
     lexOF,
     lexOUT,
     lexPACKED,
     lexPROCEDURE,
     lexPROPERTY,
     lexRECORD,
     lexRESOURCESTRING,
     lexSET,
     lexSTRING,
     lexTHREADVAR,
     lexTYPE,
     lexUNIT,
     lexUSES,
     lexVAR
  );
  TLexType = set of TTokenKind;

const LexType_Reserved = [lexARRAY..lexVAR, lexOtherReserved];
      LexType_Directive = [lexABSOLUTE..lexWRITEONLY, lexOtherDirective];
      LexType_AccessDirective = [lexAUTOMATED..lexPUBLIC];

      LexType_Ident = [lexIdent, lexSpecialFunc, lexObjDirective, lexSTRING, lexSYSTEM]+
                       LexType_Directive+LexType_AccessDirective;
      LexType_RestrictedIdent = [lexIdent, lexSpecialFunc, lexObjDirective, lexSYSTEM]+
                       LexType_Directive;

function GetToken(const ConstExpected: Boolean = False): TTokenKind;
function NameToken(const Token: TTokenKind; const RawStr: string): string; overload;
function NameToken(const Token: TTokenKind): string; overload;

function IsBoldWord(const Word: string): Boolean;

var SourceFile: TSourceFile;

    // attributes
    RawText:    string;     // "raw" source text
    StrAttrib:  string;     // string attribute
    IntAttrib:  Int64;      // integer attribute
    FltAttrib:  Extended;   // floating-point attribute

    RawAccum:   string;     // accumulated raw source text

implementation
uses SysUtils;

type
   TSpecialWord = record
                        Word: string;
                        Code: TTokenKind;
                  end;

const
   SpecialWords: array[0..120] of TSpecialWord = (
       // System unit identifier
       (Word: 'SYSTEM';     Code: lexSYSTEM),
       // operators
       (Word: 'IN';         Code: lexRelOp),
       (Word: 'IS';         Code: lexRelOp),
       (Word: 'AS';         Code: lexRelOp),
       (Word: 'OR';         Code: lexBinAddOp),
       (Word: 'XOR';        Code: lexBinAddOp),
       (Word: 'DIV';        Code: lexMulOp),
       (Word: 'MOD';        Code: lexMulOp),
       (Word: 'AND';        Code: lexMulOp),
       (Word: 'SHL';        Code: lexMulOp),
       (Word: 'SHR';        Code: lexMulOp),
       // object directives
       (Word: 'ABSTRACT';   Code: lexObjDirective),
       (Word: 'DYNAMIC';    Code: lexObjDirective),
       (Word: 'OVERRIDE';   Code: lexObjDirective),
       (Word: 'REINTRODUCE';Code: lexObjDirective),
       (Word: 'VIRTUAL';    Code: lexObjDirective),
       // access directives
       (Word: 'AUTOMATED';  Code: lexAUTOMATED),
       (Word: 'PUBLISHED';  Code: lexPUBLISHED),
       (Word: 'PRIVATE';    Code: lexPRIVATE),
       (Word: 'PROTECTED';  Code: lexPROTECTED),
       (Word: 'PUBLIC';     Code: lexPUBLIC),
       // special functions
       (Word: 'ABS';        Code: lexSpecialFunc),
       (Word: 'CHR';        Code: lexSpecialFunc),
       (Word: 'HI';         Code: lexSpecialFunc),
       (Word: 'HIGH';       Code: lexSpecialFunc),
       (Word: 'LENGTH';     Code: lexSpecialFunc),
       (Word: 'LO';         Code: lexSpecialFunc),
       (Word: 'LOW';        Code: lexSpecialFunc),
       (Word: 'ODD';        Code: lexSpecialFunc),
       (Word: 'ORD';        Code: lexSpecialFunc),
       (Word: 'PRED';       Code: lexSpecialFunc),
       (Word: 'ROUND';      Code: lexSpecialFunc),
       (Word: 'SIZEOF';     Code: lexSpecialFunc),
       (Word: 'SUCC';       Code: lexSpecialFunc),
       (Word: 'SWAP';       Code: lexSpecialFunc),
       (Word: 'TRUNC';      Code: lexSpecialFunc),
       // directives
       (Word: 'ABSOLUTE';   Code: lexABSOLUTE),
       (Word: 'DEFAULT';    Code: lexDEFAULT),
       (Word: 'DISPID';     Code: lexDISPID),
       (Word: 'EXTERNAL';   Code: lexEXTERNAL),
       (Word: 'IMPLEMENTS'; Code: lexIMPLEMENTS),
       (Word: 'INDEX';      Code: lexINDEX),
       (Word: 'MESSAGE';    Code: lexMESSAGE),
       (Word: 'NAME';       Code: lexNAME),
       (Word: 'NODEFAULT';  Code: lexNODEFAULT),
       (Word: 'READ';       Code: lexREAD),
       (Word: 'READONLY';   Code: lexREADONLY),
       (Word: 'RESIDENT';   Code: lexRESIDENT),
       (Word: 'STORED';     Code: lexSTORED),
       (Word: 'WRITE';      Code: lexWRITE),
       (Word: 'WRITEONLY';  Code: lexWRITEONLY),
       // other directives
       (Word: 'ASSEMBLER';  Code: lexOtherDirective),
       (Word: 'CDECL';      Code: lexOtherDirective),
       (Word: 'CONTAINS';   Code: lexOtherDirective),
       (Word: 'EXPORT';     Code: lexOtherDirective),
       (Word: 'FAR';        Code: lexOtherDirective),
       (Word: 'FORWARD';    Code: lexOtherDirective),
       (Word: 'NEAR';       Code: lexOtherDirective),
       (Word: 'ASSEMBLER';  Code: lexOtherDirective),
       (Word: 'OVERLOAD';   Code: lexOtherDirective),
       (Word: 'PACKAGE';    Code: lexOtherDirective),
       (Word: 'PASCAL';     Code: lexOtherDirective),
       (Word: 'REGISTER';   Code: lexOtherDirective),
       (Word: 'REQUIRES';   Code: lexOtherDirective),
       (Word: 'SAFECALL';   Code: lexOtherDirective),
       (Word: 'STDCALL';    Code: lexOtherDirective),
       // reserved words
       (Word: 'ARRAY';      Code: lexARRAY),
       (Word: 'CASE';       Code: lexCASE),
       (Word: 'CLASS';      Code: lexCLASS),
       (Word: 'CONST';      Code: lexCONST),
       (Word: 'CONSTRUCTOR';       Code: lexCONSTRUCTOR),
       (Word: 'DESTRUCTOR';        Code: lexDESTRUCTOR),
       (Word: 'DISPINTERFACE';     Code: lexDISPINTERFACE),
       (Word: 'END';               Code: lexEND),
       (Word: 'EXPORTS';           Code: lexEXPORTS),
       (Word: 'FILE';              Code: lexFILE),
       (Word: 'FUNCTION';          Code: lexFUNCTION),
       (Word: 'IMPLEMENTATION';    Code: lexIMPLEMENTATION),
       (Word: 'INTERFACE';         Code: lexINTERFACE),
       (Word: 'NIL';               Code: lexNIL),
       (Word: 'NOT';               Code: lexNOT),
       (Word: 'OBJECT';            Code: lexOBJECT),
       (Word: 'OF';                Code: lexOF),
       (Word: 'OUT';               Code: lexOUT),
       (Word: 'PACKED';            Code: lexPACKED),
       (Word: 'PROCEDURE';         Code: lexPROCEDURE),
       (Word: 'PROPERTY';          Code: lexPROPERTY),
       (Word: 'RECORD';            Code: lexRECORD),
       (Word: 'RESOURCESTRING';    Code: lexRESOURCESTRING),
       (Word: 'SET';               Code: lexSET),
       (Word: 'STRING';            Code: lexSTRING),
       (Word: 'THREADVAR';         Code: lexTHREADVAR),
       (Word: 'TYPE';              Code: lexTYPE),
       (Word: 'UNIT';              Code: lexUNIT),
       (Word: 'USES';              Code: lexUSES),
       (Word: 'VAR';               Code: lexVAR),
       // other reserved words
       (Word: 'ASM';               Code: lexOtherReserved),
       (Word: 'BEGIN';             Code: lexOtherReserved),
       (Word: 'DO';                Code: lexOtherReserved),
       (Word: 'DOWNTO';            Code: lexOtherReserved),
       (Word: 'ELSE';              Code: lexOtherReserved),
       (Word: 'EXCEPT';            Code: lexOtherReserved),
       (Word: 'FINALIZATION';      Code: lexOtherReserved),
       (Word: 'FINALLY';           Code: lexOtherReserved),
       (Word: 'FOR';               Code: lexOtherReserved),
       (Word: 'GOTO';              Code: lexOtherReserved),
       (Word: 'IF';                Code: lexOtherReserved),
       (Word: 'INHERITED';         Code: lexOtherReserved),
       (Word: 'INITIALIZATION';    Code: lexOtherReserved),
       (Word: 'INLINE';            Code: lexOtherReserved),
       (Word: 'LABEL';             Code: lexOtherReserved),
       (Word: 'LIBRARY';           Code: lexOtherReserved),
       (Word: 'PROGRAM';           Code: lexOtherReserved),
       (Word: 'RAISE';             Code: lexOtherReserved),
       (Word: 'REPEAT';            Code: lexOtherReserved),
       (Word: 'THEN';              Code: lexOtherReserved),
       (Word: 'TO';                Code: lexOtherReserved),
       (Word: 'TRY';               Code: lexOtherReserved),
       (Word: 'UNTIL';             Code: lexOtherReserved),
       (Word: 'WHILE';             Code: lexOtherReserved),
       (Word: 'WITH';              Code: lexOtherReserved)
   );

  TokenStrings: array[TTokenKind] of string = (
     '<end of file>',
     '<invalid character>',
     '',
     'integer constant',
     'floating-point constant',
     'string constant',
     'special function',
     'object directive',
     'reserved word',
     'directive',
     'SYSTEM',
     '<sign>',
     '<relop>',
     '<addop>',
     '<mulop>',
     ';',
     '=',
     ':',
     '(',
     ')',
     '..',
     '[',
     ']',
     ',',
     '^',
     '.',
     '@',
     '&',
     ':=',
     'AUTOMATED',
     'PUBLISHED',
     'PRIVATE',
     'PROTECTED',
     'PUBLIC',
     'ABSOLUTE',
     'DEFAULT',
     'DISPID',
     'EXTERNAL',
     'IMPLEMENTS',
     'INDEX',
     'MESSAGE',
     'NAME',
     'NODEFAULT',
     'READ',
     'READONLY',
     'RESIDENT',
     'STORED',
     'WRITE',
     'WRITEONLY',
     'ARRAY',
     'CASE',
     'CLASS',
     'CONST',
     'CONSTRUCTOR',
     'DESTRUCTOR',
     'DISPINTERFACE',
     'END',
     'EXPORTS',
     'FILE',
     'FUNCTION',
     'IMPLEMENTATION',
     'INTERFACE',
     'NIL',
     'NOT',
     'OBJECT',
     'OF',
     'OUT',
     'PACKED',
     'PROCEDURE',
     'PROPERTY',
     'RECORD',
     'RESOURCESTRING',
     'SET',
     'STRING',
     'THREADVAR',
     'TYPE',
     'UNIT',
     'USES',
     'VAR'
  );

// find out whether S is a special word or an ordinary identifier
function ClassifyWord(S: string): TTokenKind;
var I: Integer;
begin
     S:=UpperCase(S);
     for I:=Low(SpecialWords) to High(SpecialWords) do
         if S = SpecialWords[I].Word then
         begin
              Result := SpecialWords[I].Code;
              Exit;
         end;

     Result := lexIdent;
end;

// for the meaning of ConstExpected, see the '^' case in State=0
function GetToken(const ConstExpected: Boolean = False): TTokenKind;
var State: Byte;
    C:     Char;
    EOF:   Boolean;

    CharCode: string; // here is built up the character code after # in string constants
begin
  C:=#0;      // just to remove compiler warning

  State := 0;
  RawText := '';
  StrAttrib := '';
  IntAttrib := 0;
  FltAttrib := 0.0;

  while True do
  begin
     EOF:=SourceFile.EOF;
     if not EOF then C := SourceFile.GetChar;

     case State of
        0: begin // starting state
              if EOF then begin Result := lexEOF; Exit; end;
              case C of
                   ' ', CR, #9: {WhiteSpace - ignore};

                   'A'..'Z',
                   'a'..'z',
                   '_':         begin
                                   // identifier/special word
                                   RawText := C; 
                                   StrAttrib := C;
                                   State := 1;
                                end;

                   '0'..'9':    begin
                                   // decimal integer constant or floating-point constant
                                   RawText := C;
                                   State := 2;
                                end;

                   '$':         begin
                                   // hexadecimal integer constant
                                   RawText := C;
                                   State := 7;
                                end;

                   '''':        begin
                                   // string constant
                                   RawText := C;
                                   State := 8;
                                end;

                   '#':         begin
                                   // string constant
                                   RawText := C;
                                   State := 10;
                                end;

                   '^':         begin
                                   {
                                    BEWARE! A catch!
                                    *******
                                    PROBLEM: Object Pascal has not so simple lexical
                                    structure as it might seem. You may write something like

                                    const CRLF = ^M^J;                       (1)

                                    And, you can have something like

                                    var P: ^Integer;                         (2)

                                    Which gives two lexically COMPLETELY different tokens!
                                    In the first case, "^M^J" is a single token, whereas
                                    in the second case, ^ is one token, and Integer is the
                                    second.
                                    SOLUTION: Lexical analyzer can NOT itself decide which
                                    way is correct. So, in case syntactical analyzer is
                                    expecting a constant (which is the only situation in
                                    which the (1) case can occur), the GetToken is called
                                    with ConstExpected=True. If ConstExpected=False, the
                                    token returned is the '^' itself.
                                    NOTE: In the (1) case, ANY ASCII character may occur
                                    after '^' !
                                   }

                                   if ConstExpected then
                                   begin
                                        // case (1)
                                        RawText := C;
                                        State := 27;
                                   end else
                                   begin
                                        // case (2)
                                        Result := lexAccute;
                                        Exit;
                                   end;
                                end;

                   ';', '=', ')',
                   '[', ']', 
                   '&', '+', '-',
                   '*', ',', '@':
                                begin
                                   RawText := C;
                                   case C of
                                        ';': Result := lexSemicolon;
                                        '=': Result := lexEquals;
                                        ')': Result := lexRightRBracket;
                                        '[': Result := lexLeftABracket;
                                        ']': Result := lexRightABracket;
                                        '&': Result := lexAmpersand;
                                        '-',
                                        '+': Result := lexSign;
                                        '*': Result := lexMulOp;
                                        ',': Result := lexComma;
                                        '@': Result := lexAt;
                                        else begin
                                                  Assert(False);
                                                  Result := lexInvalid; // just to remove compiler warning
                                             end;
                                   end;
                                   Exit;
                                end;

                   ':':         begin
                                     // ':' or ':='
                                     RawText := C;
                                     State := 13;
                                end;

                   '.':         begin
                                     // '.' or '..' or '.)'
                                     RawText := C;
                                     State := 14;
                                end;

                   '<':         begin
                                     // '<' or '<>' or '<='
                                     RawText := C;
                                     State := 15;
                                end;

                   '>':         begin
                                     // '>' or '>='
                                     RawText := C;
                                     State := 16;
                                end;

                   '/':         begin
                                     // '/' or '//'
                                     RawText := C;     // When //, this / will be removed!
                                     State := 17;
                                end;

                   '{':         State := 19;

                   '(':         begin
                                     // '(' or '(.' or '(*'
                                     RawText := C;     // When (*, this ( will be removed!
                                     State := 23;
                                end;

                   else         begin
                                     RawText := C;
                                     Result := lexInvalid;
                                     Exit;
                                end;
              end;
           end;
        // ---------------------------------------------------------------------
        1: begin  // identifier or special word
              if (not EOF) and (C in ['A'..'Z', 'a'..'z', '0'..'9', '_']) then
              begin
                 RawText := RawText + C;
                 StrAttrib := StrAttrib + C;
                 Continue;
              end;
              // end of identifier
              if not EOF then SourceFile.UnGetChar(C);

              Result := ClassifyWord(StrAttrib);
              Exit;
           end;
        // ---------------------------------------------------------------------
        2: begin // decimal integer constant or floating-point constant
              if not EOF then
              begin
                case C of
                     '0'..'9': begin
                                    RawText:=RawText+C;
                                    Continue;
                               end;
                     '.':      begin
                                    RawText:=RawText+C;
                                    State:=3;
                                    Continue;
                               end;
                     'e', 'E': begin
                                    RawText:=RawText+C;
                                    State:=4;
                                    Continue;
                               end;
                end;
                SourceFile.UnGetChar(C);
              end;
              IntAttrib := StrToInt64(RawText);
              Result := lexIntConst;
              Exit;
           end;
        // ---------------------------------------------------------------------
        3: begin // floating-point constant
              if not EOF then
              begin
                case C of
                     '0'..'9': begin
                                    RawText:=RawText+C;
                                    Continue;
                               end;
                     'e', 'E': begin
                                    RawText:=RawText+C;
                                    State:=4;
                                    Continue;
                               end;
                     '.':      if RawText[Length(RawText)]='.' then
                               begin
                                    // something like "3..10"
                                    SourceFile.UnGetChar(C);
                                    SourceFile.UnGetChar('.');
                                    SetLength(RawText, Length(RawText)-1);
                                    IntAttrib := StrToInt64(RawText);
                                    Result := lexIntConst;
                                    Exit;
                               end;
                end;
                SourceFile.UnGetChar(C);
              end;
              FltAttrib := StrToFloat(RawText);
              Result := lexFloatConst;
              Exit;
           end;
        // ---------------------------------------------------------------------
        4: begin // floating-point constant (just after 'E')
              if EOF then begin Result := lexInvalid; Exit; end;
              case C of
                   '+', '-': begin
                                  RawText:=RawText+C;
                                  State:=5;
                             end;
                   '0'..'9': begin
                                  RawText:=RawText+C;
                                  State:=6;
                             end;
                   else      begin Result := lexInvalid; Exit; end;
              end;
           end;
        // ---------------------------------------------------------------------
        5: begin // floating-point constant (just after 'E+' or 'E-')
              if EOF then begin Result := lexInvalid; Exit; end;
              case C of
                   '0'..'9': begin
                                  RawText:=RawText+C;
                                  State:=6;
                             end;
                   else      begin Result := lexInvalid; Exit; end;
              end;
           end;
        // ---------------------------------------------------------------------
        6: begin // floating-point constant (exponent part)
              if (not EOF) and (C in ['0'..'9']) then
              begin
                   RawText:=RawText+C;
                   Continue;
              end;
              if not EOF then SourceFile.UnGetChar(C);

              FltAttrib := StrToFloat(RawText);
              Result := lexFloatConst;
              Exit;
           end;
        // ---------------------------------------------------------------------
        7: begin // hexadecimal integer constant
              if (not EOF) and (C in ['0'..'9', 'A'..'F', 'a'..'f']) then
              begin
                   RawText:=RawText+C;
                   Continue;
              end;
              if not EOF then SourceFile.UnGetChar(C);

              IntAttrib := StrToInt64(RawText);
              Result := lexIntConst;
              Exit;
           end;
        // ---------------------------------------------------------------------
        8: begin // string constant (inside quotes)
              if EOF then begin Result:=lexInvalid; Exit; end;
              RawText := RawText + C;
              if C='''' then State:=9
                        else StrAttrib := StrAttrib + C;
           end;
        // ---------------------------------------------------------------------
        9: begin // string constant (just after closing quote or after ^X)
              if (not EOF) then
              begin
                   case C of
                        '''': begin
                                   RawText := RawText + C;
                                   StrAttrib := StrAttrib + C;
                                   State := 8;
                                   Continue;
                              end;
                        '#':  begin
                                   RawText := RawText + C;
                                   State := 10;
                                   Continue;
                              end;
                        '^': begin
                                  RawText := RawText + C;
                                  State := 27;
                                  Continue;
                             end;
                   end;
                   SourceFile.UnGetChar(C);
              end;
              Result := lexStrConst;
              Exit;
           end;
        // ---------------------------------------------------------------------
        10: begin // string constant (just after '#')
              if (not EOF) then
              begin
                   case C of
                        '''': begin
                                   RawText := RawText + C;
                                   StrAttrib := StrAttrib + #0;
                                   State := 8;
                                   Continue;
                              end;
                        '^':  begin
                                   RawText := RawText + C;
                                   StrAttrib := StrAttrib + #0;
                                   State := 27;
                                   Continue;
                              end;
                        '#':  begin
                                   RawText := RawText + C;
                                   StrAttrib := StrAttrib + #0;
                                   Continue;
                              end;
                        '$':  begin
                                   RawText := RawText + C;
                                   CharCode := C;
                                   State := 12;
                                   Continue;
                              end;
                        '0'..'9':
                              begin
                                   RawText := RawText + C;
                                   CharCode := C;
                                   State := 11;
                                   Continue;
                              end;
                   end;
                   SourceFile.UnGetChar(C);
              end;
              StrAttrib := StrAttrib + #0;
              Result := lexStrConst;
              Exit;
            end;
        // ---------------------------------------------------------------------
        11: begin // string constant (decimal character code)
              if (not EOF) then
              begin
                   case C of
                        '0'..'9':
                              begin
                                   RawText := RawText + C;
                                   CharCode := CharCode + C;
                                   Continue;
                              end;
                        '''': begin
                                   RawText := RawText + C;
                                   StrAttrib := StrAttrib + Char(StrToInt(CharCode));
                                   State := 8;
                                   Continue;
                              end;
                        '#':  begin
                                   RawText := RawText + C;
                                   StrAttrib := StrAttrib + Char(StrToInt(CharCode));
                                   State := 10;
                                   Continue;
                              end;
                        '^':  begin
                                   RawText := RawText + C;
                                   StrAttrib := StrAttrib + Char(StrToInt(CharCode));
                                   State := 27;
                                   Continue;
                              end;
                   end;
                   SourceFile.UnGetChar(C);
              end;
              StrAttrib := StrAttrib + Char(StrToInt(CharCode));
              Result := lexStrConst;
              Exit;
            end;
        // ---------------------------------------------------------------------
        12: begin // string constant (hexadecimal character code)
              if (not EOF) then
              begin
                   case C of
                        '0'..'9',
                        'A'..'F',
                        'a'..'f':
                              begin
                                   RawText := RawText + C;
                                   CharCode := CharCode + C;
                                   Continue;
                              end;
                        '''': begin
                                   RawText := RawText + C;
                                   StrAttrib := StrAttrib + Char(StrToInt(CharCode));
                                   State := 8;
                                   Continue;
                              end;
                        '#':  begin
                                   RawText := RawText + C;
                                   StrAttrib := StrAttrib + Char(StrToInt(CharCode));
                                   State := 10;
                                   Continue;
                              end;
                        '^':  begin
                                   RawText := RawText + C;
                                   StrAttrib := StrAttrib + Char(StrToInt(CharCode));
                                   State := 27;
                                   Continue;
                              end;
                   end;
                   SourceFile.UnGetChar(C);
              end;
              StrAttrib := StrAttrib + Char(StrToInt(CharCode));
              Result := lexStrConst;
              Exit;
            end;
        // ---------------------------------------------------------------------
      //27: after '^' in string constant (see below)
        // ---------------------------------------------------------------------
        13: begin // : or :=
              if not EOF then
                 if C = '=' then begin
                                      RawText := RawText + C;
                                      Result := lexAssign;
                                 end
                            else begin
                                      Result := lexColon;
                                      SourceFile.UnGetChar(C);
                                 end
              else
                 Result := lexColon;

              Exit;
            end;
        // ---------------------------------------------------------------------
        14: begin // . or .. or .)
              if not EOF then
                 case C of
                      '.': begin
                                RawText := RawText + C;
                                Result := lexDotDot;
                           end;
                      ')': begin
                                RawText := RawText + C;
                                Result := lexRightABracket;
                           end;
                      else begin
                                Result := lexDot;
                                SourceFile.UnGetChar(C);
                           end;
                 end
              else
                 Result := lexDot;

              Exit;
            end;
        // ---------------------------------------------------------------------
        15: begin // < or <= or <>
              if not EOF then
                 if C in ['=', '>'] then RawText := RawText + C
                                    else SourceFile.UnGetChar(C);
              Result := lexRelOp;
              Exit;
            end;
        // ---------------------------------------------------------------------
        16: begin // > or >=
              if not EOF then
                 if C = '=' then RawText := RawText + C
                            else SourceFile.UnGetChar(C);
              Result := lexRelOp;
              Exit;
            end;
        // ---------------------------------------------------------------------
        17: begin // / or //
              if not EOF then
              begin
                if C='/' then
                begin
                   Assert(RawText[Length(RawText)]='/');
                   SetLength(RawText, Length(RawText)-1); // remove the first '/'
                   State := 18;
                   Continue;
                end;
                SourceFile.UnGetChar(C);
              end;
              Result := lexMulOp;
              Exit;
            end;
        // ---------------------------------------------------------------------
        18: begin // '//' comment
              if EOF then begin Result := lexEOF; Exit; end;
              if C=CR then State:=0;
            end;
        // ---------------------------------------------------------------------
        19: begin // inside { } comment
              if EOF then begin Result := lexInvalid; Exit; end;
              case C of
                   '}': State := 0;
                   '(': State := 20;
              end;
            end;
        // ---------------------------------------------------------------------
        20: begin // inside { } comment, after ( character
              if EOF then begin Result := lexInvalid; Exit; end;
              case C of
                   '(': ;
                   '*': State := 21;
                   else State := 19;
              end;
            end;
        // ---------------------------------------------------------------------
        21: begin // inside { (* *) } comment
              if EOF then begin Result := lexInvalid; Exit; end;
              if C = '*' then State := 22;
            end;
        // ---------------------------------------------------------------------
        22: begin // inside { (* *) } comment, after * character
              if EOF then begin Result := lexInvalid; Exit; end;
              case C of
                   '*': ;
                   ')': State := 19;
                   else State := 21;
              end;
            end;
        // ---------------------------------------------------------------------
        23: begin // ( or (. or (*
              if not EOF then
              begin
                case C of
                     '.': begin
                               RawText := RawText + C;
                               Result := lexLeftABracket;
                               Exit;
                          end;
                     '*': begin
                               Assert(RawText[Length(RawText)]='(');
                               SetLength(RawText, Length(RawText)-1); // remove the preceeding '('
                               State := 24;
                               Continue;
                          end;
                end;
                SourceFile.UnGetChar(C);
              end;
              Result := lexLeftRBracket;
              Exit;
            end;
        // ---------------------------------------------------------------------
        24: begin // inside (* *) comment
              if EOF then begin Result := lexInvalid; Exit; end;
              case C of
                   '{': State := 25;
                   '*': State := 26;
              end;
            end;
        // ---------------------------------------------------------------------
        25: begin // inside (* { } *) comment
              if EOF then begin Result := lexInvalid; Exit; end;
              if C = '}' then State := 24;
            end;
        // ---------------------------------------------------------------------
        26: begin // inside (* *) comment, after * character
              if EOF then begin Result := lexInvalid; Exit; end;
              case C of
                   '*': ;
                   ')': State := 0;
                   else State := 24;
              end;
            end;
        // ---------------------------------------------------------------------
        27: begin // after '^' in string constant
              if EOF then begin Result := lexInvalid; Exit; end;
              RawText := RawText + C;
              StrAttrib := StrAttrib + Char(Byte(C) xor 64);
              State := 9;
            end;
        // ---------------------------------------------------------------------
        else Assert(False);
     end;
  end;
end;

function NameToken(const Token: TTokenKind; const RawStr: string): string; overload;
begin
     if Token in LexType_Reserved then Result := '''' + UpperCase(RawStr) + ''''
     else if Token in (LexType_Directive + LexType_AccessDirective +
                       [lexIdent, lexSpecialFunc, lexObjDirective, lexSYSTEM]) then
                       if RawStr<>'' then Result := 'identifier ''' + RawStr + ''''
                                     else Result := 'identifier'
     else case Token of
               lexEOF: Result := 'end of file';
               lexInvalid: Result := 'syntax error';
               lexIntConst: Result := 'integer constant';
               lexFloatConst: Result := 'floating-point constant';
               lexStrConst: Result := 'string constant';
               else Result := '''' + UpperCase(RawStr) + ''''; // operators
          end;
end;

function NameToken(const Token: TTokenKind): string; overload;
begin
     Result := NameToken(Token, TokenStrings[Token]);
end;

function IsBoldWord(const Word: string): Boolean;
var T: TTokenKind;
begin
     T := ClassifyWord(Word);
     Result := T in (LexType_Reserved+LexType_Directive+LexType_AccessDirective);
end;

initialization
  DecimalSeparator := '.';
end.
