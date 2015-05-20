{
  Priklad k semestralni praci z PJ.
  Petr Kadlec <kadlecp2@fel.cvut.cz>

  Tento priklad ukazuje hlavne spravnost syntakticke analyzy zdrojoveho textu,
  obsahuje proto mnoho ruznych syntaktickych prvku.
}

unit TestUnit;

interface

var Cislo: Pointer = nil;

const MAX_LEN = $3;                   // hexadecimalni konstanta

type TArrayIndex = 0..MAX_LEN-1;

     TRec = packed record             // zaznam s variantni casti, s vlozenou variantni casti
                   Cislo: Integer;
                   Vstup: string;
                   Neco:  1..15;
                   case Integer of
                        0: (A: Single);
                        1: (case Switch: Byte of
                                 1+3: (B: Cardinal);
                                 15:  (C: Char)
                           )
            end;

     TChild = class;                  // forward deklarace
     EVyjimka = class(TObject);       // prosty subclassing

     TParent = class
               private
                    FPrvek: string;
                    FDite: tchild;
               published
                    property Prvek: string read FPrvek;
                    property Dite: TChild write FDite;
               end;

     TChild = class(TParent)
              private
                    FPridano: Integer;
              public
                    property Pridano: Integer read FPridano write FPridano;
              end;

     IParent = interface
                 function Ahoj: string;
                 property Pozdrav: string read Ahoj;
               end;

     IChild = interface(IParent)
                function Pridat: Integer;
              end;

var CurrentIndex: TArrayIndex;

const
      Jmena: Array[TArrayIndex] of String = (
             'Petr+Honza', 'Pavel', 'folder\tab'
      );

exports Jmena;

resourcestring
  strAhoj = 'Ahoj';
  strWarning = 'Varování';

procedure Globalni_Ahoj;
procedure InputCallback(var Input: string; const Default: Boolean = True); stdcall;

implementation

... ignorovano ...

