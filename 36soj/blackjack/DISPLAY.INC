; DISPLAY.INC - modul pro praci s grafikou - rozhrani
; (c) 2000 by DragonSoft

; vlastni kod je v 'DISPLAY.ASM'

.DATA
EXTRN TextColor:BYTE
.CODE
EXTRN _InitDisplay:PROC,_CloseDisplay:PROC,_DisplayImage:PROC,_FillRect:PROC,_CharOut:PROC,_TextOut:PROC,_IntOut:PROC

; konstanty barev
White       EQU 1
Black       EQU 0
Red         EQU 2

.CODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DisplayImage - zobrazi obrazek na obrazovce
;;
;; parametry:
;;    PicData:   near pointer na data obrazku
;;    X, Y:      souradnice leveho horniho rohu obrazku na obrazovce
;;    PicWidth,
;;    PicHeight: sirka a vyska obrazku
;;
;; navratova hodnota: zadna
;; modifikuje: AX, BX, CX, SI, DI, ES, Flags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DisplayImage MACRO PicData, X, Y, PicWidth, PicHeight
    PUSH PicHeight
    PUSH PicWidth
    PUSH Y
    PUSH X
    PUSH OFFSET PicData
    CALL _DisplayImage
ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ClrScr - smaze obrazovku
;;
;; parametry: zadne
;; navratova hodnota: zadna
;; modifikuje: AX, BX, CX, DI, ES, Flags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ClrScr MACRO X1, Y1, X2, Y2, Color
    PUSH WORD PTR Black ; cerne pozadi
    PUSH 199
    PUSH 319
    PUSH WORD PTR 0
    PUSH WORD PTR 0
    CALL _FillRect
ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FillRect - vyplni na obrazovce obdelnik
;;
;; parametry:
;;    X1, Y1:    souradnice leveho horniho rohu obdelniku na obrazovce
;;    X2, Y2:    souradnice praveho dolniho rohu obdelniku na obrazovce
;;    Color:     barva, kterou se ma obdelnik vyplnit (pouzit pouze dolni byte)
;;
;; navratova hodnota: zadna
;; modifikuje: AX, BX, CX, DI, ES, Flags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FillRect MACRO X1, Y1, X2, Y2, Color
    PUSH Color
    PUSH Y2
    PUSH X2
    PUSH Y1
    PUSH X1
    CALL _FillRect
ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CharOut - vypise na obrazovku jeden znak
;;
;; parametry:
;;    X, Y:      souradnice leveho horniho rohu znaku
;;    Character: znak k vypsani
;;
;; navratova hodnota: zadna
;; modifikuje: AX, BX, CX, SI, DI, ES, Flags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CharOut MACRO X, Y, Character
    PUSH Character
    PUSH Y
    PUSH X
    CALL _CharOut
ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TextOut - vypise na obrazovku ASCIIZ retezec
;;
;; parametry:
;;    X, Y:    souradnice leveho horniho rohu textu
;;    Text:    ASCIIZ text k vypsani
;;
;; navratova hodnota: zadna
;; modifikuje: AX, BX, CX, DX, SI, DI, ES, Flags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
TextOut MACRO X, Y, Text
    PUSH OFFSET Text
    PUSH Y
    PUSH X
    CALL _TextOut
ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IntOut - vypise na obrazovku cislo
;;
;; parametry:
;;    X, Y:    souradnice leveho horniho rohu textu
;;    Num:     cislo k vypsani
;;
;; navratova hodnota: zadna
;; modifikuje: AX, BX, CX, DX, DI, ES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
IntOut MACRO X, Y, Num
    PUSH Num
    PUSH Y
    PUSH X
    CALL _IntOut
ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; InitDisplay - inicializuje graficky rezim
;;
;; parametry: zadne
;; navratova hodnota: zadna
;; modifikuje: AX, BX, CX, DX, ES, Flags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
InitDisplay MACRO
    CALL _InitDisplay
ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CloseDisplay - ukonci graficky rezim
;;
;; parametry: zadne
;; navratova hodnota: zadna
;; modifikuje: AX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CloseDisplay MACRO
    CALL _CloseDisplay
ENDM
