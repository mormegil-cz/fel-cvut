; DISPLAY.ASM - modul pro praci s grafikou
; (c) 2000 by DragonSoft

; rozhrani a dokumentace rozhrani v 'DISPLAY.INC'

INCLUDE Common.inc

.CONST

VideoSeg        EQU 0A000h      ; segment VGA videopameti
ScreenPitch     EQU 320         ; delka radku obrazovky (byte)

FontWidth       EQU 8           ; sirka fontu (NELZE jen zmenit konstantu!)
FontHeight      EQU 8           ; vyska fontu

InvalidFont     DB "Nepodporovany ROM BIOS font 8x8",10,13,"$"

.DATA

PUBLIC TextColor

OldMode   DB 3                  ; puvodni videorezim
TextColor DB 1                  ; barva textu

.DATA?

FontOfs DW ?                    ; adresa fontu (8x14)
FontSeg DW ?

StrOut DB 6 DUP (?)             ; docasna promenna pro zobrazovani cisel

.CODE

PUBLIC _InitDisplay
PUBLIC _CloseDisplay
PUBLIC _DisplayImage
PUBLIC _FillRect
PUBLIC _CharOut
PUBLIC _TextOut
PUBLIC _IntOut

_DisplayImage PROC NEAR
ARG PicData:NEAR PTR BYTE, X:WORD, Y:WORD, PicWidth:WORD, PicHeight:WORD = ArgLen

    ENTER 0, 0

    PUSH VideoSeg
    POP  ES

    MOV  SI, PicData

    MOV  AX, Y                  ; adresa zapisovaneho pixelu
    MOV  DI, AX                 ; =Y*320 + X
    SHL  DI, 6                  ; (Y*64)
    XCHG AH, AL                 ; (Y*256)
    ADD  DI, AX                 ; (Y*320)
    ADD  DI, X                  ;        + X

    CLD

    MOV  BX, PicHeight          ; BX = citac vnejsi smycky (radky)
    MOV  CX, PicWidth           ; CX = citac vnitrni smycky (sloupce)
@@Line:
    PUSH CX
    REP  MOVSB                  ; Zobrazeni jedne radky
    POP  CX

    ADD  DI, ScreenPitch        ; posun na dalsi radku na obrazovce
    SUB  DI, CX

    DEC  BX
    JNZ  @@Line

    LEAVE
    RET  ArgLen
ENDP

_FillRect PROC NEAR
ARG X1:WORD, Y1:WORD, X2:WORD, Y2:WORD, Color:WORD = ArgLen

    ENTER 0, 0

    PUSH VideoSeg
    POP  ES

    MOV  AX, Y1                 ; adresa zapisovaneho pixelu
    MOV  DI, AX                 ; =Y1*320 + X1
    SHL  DI, 6                  ; (Y1*64)
    XCHG AH, AL                 ; (Y1*256)
    ADD  DI, AX                 ; (Y1*320)
    ADD  DI, X1                 ;        + X1

    CLD

    MOV  BX, Y2                 ; BX = citac vnejsi smycky (radky)
    SUB  BX, Y1
    INC  BX
    MOV  CX, X2                 ; CX = citac vnitrni smycky (sloupce)
    SUB  CX, X1
    INC  CX
    MOV  AX, Color
@@Line:
    PUSH CX
    REP  STOSB                  ; Zobrazeni jedne radky
    POP  CX

    ADD  DI, ScreenPitch        ; posun na dalsi radku na obrazovce
    SUB  DI, CX

    DEC  BX
    JNZ  @@Line

    LEAVE
    RET  ArgLen
ENDP

_CharOut PROC NEAR
ARG X:WORD, Y:WORD, Character:WORD = ArgLen

    ENTER 0, 0

    PUSH  VideoSeg
    POP   ES

    MOV  AX, Y                  ; adresa zapisovaneho pixelu
    MOV  DI, AX                 ; =Y*320 + X
    SHL  DI, 6                  ; (Y*64)
    XCHG AH, AL                 ; (Y*256)
    ADD  DI, AX                 ; (Y*320)
    ADD  DI, X                  ;        + X

    MOV  BH, TextColor          ; BH = barva textu

    CLD

    PUSH DS

    MOV  AX, FontSeg
    MOV  SI, FontOfs
    MOV  DS, AX

    MOV  CX, Character
    XOR  CH, CH
    SHL  CX, 3                  ; lg(FontHeight)
    ADD  SI, CX

    MOV  CX, FontHeight
@@NextLine:
    PUSH CX
    LODSB
    MOV  DL, AL                 ; DL = font data
    MOV  AL, BH                 ; AL = barva textu
    MOV  BL, 80h                ; BL = bitova maska
    MOV  CX, FontWidth
@@NextBit:
    TEST DL, BL
    JZ   @@SkipBit
    MOV  ES:[DI], AL
@@SkipBit:
    SHR  BL, 1
    INC  DI
    LOOP @@NextBit

    ADD  DI, ScreenPitch-FontWidth

    POP  CX
    LOOP @@NextLine

    POP  DS

    LEAVE
    RET  ArgLen
ENDP

_TextOut PROC NEAR
ARG X:WORD, Y:WORD, Text:NEAR PTR BYTE = ArgLen

    ENTER 0, 0

    MOV   SI, Text
    MOV   DX, X
@@NextChar:
    LODSB
    TEST  AL, AL
    JZ    @@EndText

    PUSH  SI
    PUSH  DX

    PUSH  AX
    PUSH  Y
    PUSH  DX
    CALL  _CharOut

    POP   DX
    POP   SI
    ADD   DX, FontWidth
    JMP   @@NextChar

@@EndText:
    LEAVE
    RET  ArgLen
ENDP

_InitDisplay PROC NEAR
  MOV  AH, 0Fh                  ; Cti graficky rezim
  INT  10h
  MOV  OldMode, AL

  MOV  AX, 0013h                ; Nastav rezim 13h = 320x200x256
  INT  10h

  PUSH BP
  MOV  AX, 1130h                ; Vrat informace o fontu
  MOV  BH, 3                    ; 8x8
  INT  10h
  MOV  FontOfs, BP
  MOV  FontSeg, ES
  POP  BP

  CMP  CX, FontHeight           ; je to klasicky 8x14 ?
  JE   @@Exit

  MOV  AL, OldMode              ; ne=>ukonci s chybou
  XOR  AH, AH
  INT  10h

  MOV  AH, 9
  MOV  DX, OFFSET InvalidFont
  INT  21h

  EXITCODE 1
@@Exit:
  RET
ENDP

_CloseDisplay PROC NEAR
  XOR  AH, AH                   ; Nastav rezim
  MOV  AL, OldMode
  INT  10h

  RET
ENDP

_IntOut PROC NEAR
ARG X:WORD, Y:WORD, Num:WORD = ArgLen

    ENTER 0, 0

    MOV  DI, OFFSET StrOut[6]           ; konverze do retezce StrOut
    ;MOV  BYTE PTR [DI], "0"
    ;DEC  DI
    XOR  CX, CX
    MOV  BX, 10
    MOV  AX, Num
@@NextNum:
    XOR  DX, DX
    DIV  BX                             ; dalsi desitkovy rad cisla

    ADD  DL, "0"                        ; prevod cisla do ASCII
    MOV  [DI], DL
    DEC  DI

    INC  CX

    TEST AX, AX
    JNZ  @@NextNum

@@StopIt:
    MOV  SI, DI                         ; prenos na zacatek StrOut
    INC  SI
    MOV  DI, OFFSET StrOut
    PUSH DI                             ; pro pozdejsi TextOut
    PUSH DS
    POP  ES
    CLD
    ;INC  CX
    REP  MOVSB
    XOR  AL, AL
    STOSB                               ; #0 na konci ASCIIZ

    PUSH Y                              ; pouzij TextOut k vypisu
    PUSH X
    CALL _TextOut

    LEAVE
    RET  ArgLen
ENDP

END