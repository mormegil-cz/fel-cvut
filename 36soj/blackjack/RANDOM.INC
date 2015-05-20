; RANDOM.INC - modul s generatorem nahodnych cisel - rozhrani
; (c) 2000 by DragonSoft

EXTRN _Random:PROC,_Randomize:PROC
; vlastni kod je v 'RANDOM.ASM'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Random - vrati nahodne cislo
;;
;; parametry:
;;    Range:    rozsah nahodnych cisel
;;
;; navratova hodnota:
;;    AX = nahodne cislo v rozsahu 0 <= X < Range
;;
;; modifikuje: <AX>, BX, CX, DX, Flags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Random MACRO Range
       PUSH Range
       CALL _Random
ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Randomize - inicializuje generator nahodnych cisel
;;
;; parametry: zadne
;; navratova hodnota: zadna
;;
;; modifikuje: AX, CX, DX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Randomize MACRO
          CALL _Randomize
ENDM
