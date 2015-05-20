; MOUSER.INC - modul pro praci s mysi - rozhrani
; (c) 2000 by DragonSoft

EXTRN _InitMouse:PROC,_GetMouseAction:PROC,_EnableCursor:PROC,_DisableCursor:PROC
EXTRN MouseX:WORD,MouseY:WORD,MouseBtn:WORD
; vlastni kod je v 'MOUSER.ASM'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; InitMouse - inicializuje ovladac mysi
;;
;; parametry: zadne
;; navratova hodnota: zadna, pri nenalezenem ovladaci ukonci program
;;
;; modifikuje: AX, BX, Flags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
InitMouse MACRO
       CALL _InitMouse
ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GetMouseAction - pocka na stisk mysitka a vrati pozici
;;
;; parametry: zadne
;; navratova hodnota: zadna, pri stisku ESC ukonci program
;;                    informace vraci v MouseX, MouseY, MouseBtn
;;
;; modifikuje: AX, BX, CX, DX, Flags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GetMouseAction MACRO
       CALL _GetMouseAction
ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EnableCursor - zapne zobrazovani kurzoru
;;
;; parametry: zadne
;; navratova hodnota: zadna
;;
;; modifikuje: AX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
EnableCursor MACRO
       CALL _EnableCursor
ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DisableCursor - vypne zobrazovani kurzoru
;;
;; parametry: zadne
;; navratova hodnota: zadna
;;
;; modifikuje: AX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DisableCursor MACRO
       CALL _DisableCursor
ENDM
