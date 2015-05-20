; TGA.INC - modul pro praci s TGA obrazky - rozhrani
; (c) 2000 by DragonSoft

; vlastni kod je v 'TGA.ASM'

EXTRN _LoadTGA:PROC,_SetPalette:PROC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LoadTGA - nahraje TGA obrazek do pameti
;;
;; parametry:
;;    FileName: near pointer na ASCIIZ jmeno souboru
;;    PicData:  near pointer na misto pro data obrazku
;;
;; navratova hodnota:
;;    AX == 0 ... OK
;;    AX <> 0 ... chyba
;;
;; modifikuje: <AX>, BX, CX, DX, SI, DI, Flags, ES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
LoadTGA MACRO FileName, PicData
        PUSH OFFSET PicData
        PUSH OFFSET FileName
        CALL _LoadTGA
ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SetPalette - aktivuje paletu ziskanou z TGA
;;
;; parametry: zadne
;; navratova hodnota: zadna
;; modifikuje: AL, CX, DX, SI, Flags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetPalette MACRO
        CALL _SetPalette
ENDM
