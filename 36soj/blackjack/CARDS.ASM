; CARDS.ASM - modul pro praci s kartami
; (c) 2000 by DragonSoft

; rozhrani a dokumentace rozhrani v 'CARDS.INC'

INCLUDE Common.inc

.CONST
CardName  DB "?A23456789DJQK"
ColorName DB ""

CardWidth  EQU 40
CardHeight EQU 60

ReverseFName DB "REVERSE.TGA",0

.DATA?
ReverseTGA DB CardWidth*CardHeight DUP (?)

.CODE

PUBLIC _InitCards
PUBLIC _PaintCard
PUBLIC _PaintReverse

INCLUDE Display.inc
INCLUDE TGA.inc

_InitCards PROC NEAR
    LoadTGA ReverseFName, ReverseTGA
    SetPalette
    RET
ENDP

_PaintCard PROC NEAR
ARG X:WORD, Y:WORD, Card:WORD = ArgLen

    ENTER 0, 0

    MOV   AX, X                         ; cerny ramecek karty
    ADD   AX, CardWidth
    DEC   AX
    MOV   BX, Y
    ADD   BX, CardHeight
    DEC   BX
    FillRect X,Y,AX,BX,Black

    MOV   AX, X                         ; bila vypln karty
    MOV   CX, AX
    INC   CX
    ADD   AX, CardWidth
    DEC   AX
    DEC   AX
    MOV   BX, Y
    MOV   DX, BX
    INC   DX
    ADD   BX, CardHeight
    DEC   BX
    DEC   BX
    FillRect CX,DX,AX,BX,White

    MOV   AX, X                         ; hodnota karty
    INC   AX                            ; [X+2,Y+2]
    INC   AX
    MOV   BX, Y
    INC   BX
    INC   BX
    MOV   SI, Card

    MOV   TextColor, Black              ; zvol spravnou barvu textu
    TEST  SI, 01h
    JNZ   @@IsBlackColor
    MOV   TextColor, Red
@@IsBlackColor:

    AND   SI, 0FFh
    PUSH  SI
    SHR   SI, 4
    MOV   DL, CardName[SI]
    POP   SI
    PUSH  AX
    PUSH  BX
    PUSH  SI
    CharOut AX, BX, DX
    POP   SI
    POP   BX
    POP   AX

    ADD   AX, 15                        ; barva karty
    ADD   BX, 28                        ; [X+17, Y+30]
    AND   SI, 0Fh
    MOV   DL, ColorName[SI]
    CharOut AX, BX, DX

    LEAVE
    RET  ArgLen

ENDP

_PaintReverse PROC NEAR
ARG X:WORD, Y:WORD = ArgLen

    ENTER 0, 0

    DisplayImage ReverseTGA, X, Y, CardWidth, CardHeight

    LEAVE
    RET  ArgLen
ENDP

END