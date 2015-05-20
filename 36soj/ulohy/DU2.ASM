; 2. Domaci ukol ze SOJ
; Petr Kadlec, cviceni ze SOJ ve ctvrtek od 9:15
; Zvolene zadani:
;         po zadani obrazu cisel 0-9 do matice 8x8 tato cisla zobrazujte

CODE SEGMENT
ASSUME CS:CODE, DS:CODE, ES:CODE

.286

ORG 100h

ScreenWidth  EQU 80     ; Velikost obrazovky (znaku)
ScreenHeight EQU 25

Rows    EQU 8           ; Radku v matici fontu (mocnina 2!)
Cols    EQU 8           ; Sloupcu v matici fontu (mocnina 2!)

LgCharL EQU 6           ; Lg(Rows*Cols)
MinChar EQU "0"         ; Prvni znak k editaci
MaxChar EQU "9"         ; Posledni znak k editaci
LowChar EQU " "         ; Nejmensi znak, ktery smi byt v matici fontu
HiChar  EQU "9"         ; Nejvetsi znak, ktery smi byt v matici fontu

Start:
      CLD

      ; *** EDITACE FONTU ***
      MOV   DI, OFFSET FontMatrix   ; na [DI] je matice fontu
      MOV   DL, MinChar             ; for DL:=MINCHAR to MAXCHAR do
EditChar:
      CALL  CLS

      PUSH  DX                      ; vypise hlavicku "Editovany znak: "
      MOV   AH, 9
      MOV   DX, OFFSET Header
      INT   21h
      POP   DX

      CALL  PrintChar
      CALL  PrintCRLF
      MOV   DH, Rows                ; for DH:=ROWS downto 1 do
EditRow:
      PUSH  DX
      MOV   CX, Cols                ; for CX:=COLS downto 1 do
InKey:
      MOV   AH, 8               ; Vstup znaku
      INT   21h

      CMP   AL, 27
      JNE   Continue
      POP   DX
      JMP   FontDisplay
Continue:

      CMP   AL, LowChar         ; Je to znak povoleny ve fontu?
      JB    InKey                 ; ne
      CMP   AL, HiChar
      JA    InKey                 ; ne

      STOSB                     ; OK, ulozit do matice fontu
      MOV   DL, AL              ; a vypiseme ho
      CALL  PrintChar

      LOOP  InKey               ; Next CX
      CALL  PrintCRLF           ; Dalsi radek matice fontu

      POP   DX
      DEC   DH                  ; Next DH
      JNZ   EditRow

      INC   DL                  ; Next DL
      CMP   DL, MaxChar
      JBE   EditChar

      ; *** ZOBRAZOVANI FONTU ***
FontDisplay:
      CALL  CLS
DisplayFont:
      MOV   AH, 8               ; Vstup znaku
      INT   21h

      CMP   AL, 27              ; ESC?
      JE    Exit                ;  => konec

      CMP   AL, MaxChar         ; Mame pro tento znak definovany font?
      JA    DisplayFont
      SUB   AL, MinChar
      JB    DisplayFont
      ; Je to OK, zobrazime ho
      CBW                       ; vypocitame zacatek definice znaku ve fontu
      SHL   AX, LgCharL
      MOV   SI, OFFSET FontMatrix
      ADD   SI, AX              ; v SI je zacatek tohoto znaku
      CLD
      MOV   DH, ScreenHeight-Rows
                                ; v DH je prave zobrazovana "scanline"
      XOR   BX, BX              ; BH=videostranka pro sluzbu INT 10h
                                ; BL=barva podkladu pro INT 10h
DisplayRow:
      MOV   DL, LevaPozice      ; v DL je sloupec, na ktery zobrazujeme
      MOV   AH, 2               ; sluzba INT 10h: Nastav pozici kurzoru
      INT   10h
      MOV   CX, Cols            ; v CX je pocitadlo sloupcu prislusneho znaku
      MOV   AH, 0Eh             ; sluzba INT 10h: Napis znak (emuluj TTY)
DisplayChar:
      LODSB                     ; vezmi znak k zapisu
      INT   10h                 ; vypis
      LOOP  DisplayChar         ; takhle zobraz cely radek
      INC   DH                  ; dalsi radek?
      CMP   DH, ScreenHeight
      JNE   DisplayRow

      ADD   LevaPozice, Cols+1  ; posuneme se o znak doprava
      CMP   LevaPozice, ScreenWidth-Cols
      JB    DisplayFont         ; pokud jsme uz na kraji obrazovky,

      MOV   AX, 0600h + Rows+1  ; odscrollujeme o radek nize
      XOR   CX, CX
      MOV   DX, (ScreenHeight-1) * 100h + (ScreenWidth-1)
      MOV   BH, 7
      INT   10h
      MOV   LevaPozice, 0       ; a zacneme od zacatku radky

      JMP   DisplayFont         ; a pokracujeme...

Exit: RET                       ; KONEC

CLS   PROC NEAR         ; Smazani obrazovky

      IRP  data, <BX, DX, ES, DI, CX, 0B800h>
           PUSH data
      ENDM

      POP  ES
      XOR  DI, DI
      MOV  CX, 80*25
      MOV  AX, 700H
      REP  STOSW

      MOV  AH, 2        ; +kurzor na (0,0)
      XOR  BH, BH
      XOR  DX, DX
      INT  10h

      IRP  reg, <CX, DI, ES, DX, BX>
           POP  reg
      ENDM

      RET
ENDP

PrintChar PROC NEAR     ; Vytiskne znak v DL
      MOV  AH, 2
      INT  21h
      RET
ENDP

PrintCRLF PROC NEAR     ; Odradkuje
      PUSH DX
      MOV  AH, 9
      MOV  DX, OFFSET CRLF
      INT  21h
      POP  DX
      RET
ENDP

Header DB "Editovany znak: $"
CRLF   DB 13,10,"$"

LevaPozice DB 0
FontMatrix: ;DB ((MaxChar-MinChar+1)*Rows*Cols) DUP (?)

           ; Nejaky preddefinovany font pro testovani

           DB " 000000 "
           DB "0      0"
           DB "0      0"
           DB "0      0"
           DB "0      0"
           DB "0      0"
           DB "0      0"
           DB " 000000 "

           DB "    1   "
           DB "   11   "
           DB "  1 1   "
           DB " 1  1   "
           DB "    1   "
           DB "    1   "
           DB "    1   "
           DB "    1   "

           DB " 2222   "
           DB "2    2  "
           DB "2    2  "
           DB "    2   "
           DB "   2    "
           DB "  2     "
           DB " 2      "
           DB "222222  "

           DB " 3333   "
           DB "     3  "
           DB "    3   "
           DB "  33    "
           DB "    3   "
           DB "     3  "
           DB "     3  "
           DB " 3333   "

CODE ENDS
END START
