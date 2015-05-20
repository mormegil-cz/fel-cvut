; LOGICS.INC - modul pro logiku hry
; (c) 2000 by DragonSoft

; vlastni kod je v 'LOGICS.ASM'

.CONST

MAX_PLAYER_CARDS        EQU 10           ; 4x2 + 4x3 + 1xA + (BUST)
MAX_PLAYERS             EQU 10           ; nepouzito

CARD_COLORS             EQU 4            ; 4 barvy
CARD_VALUES             EQU 13           ; 13 hodnot

CARD_DECK_COUNT         EQU 1                        ; 1-balickova hra
CARD_DECK_SIZE          EQU CARD_COLORS*CARD_VALUES  ; karet v balicku

CARD_COUNT              EQU CARD_DECK_COUNT*CARD_DECK_SIZE  ; karet celkem

BLACK_JACK_COUNT        EQU 21                       ; cil hry
MIN_STAND_SUM           EQU 12                       ; minimalni soucet pro STAND
DEALER_STAND_SUM        EQU 17                       ; minimalni koncovy soucet bankere

.DATA?

TPlayer STRUC
             Sum       DB ?
             CardCount DW ?
             Cards     DB MAX_PLAYER_CARDS DUP (?)
ENDS

EXTRN Bet:WORD, Player:TPlayer, Dealer:TPlayer, ShowDealerCards:BYTE

.DATA

EXTRN Cash:WORD,DeckPos:WORD

.CODE

EXTRN _RedrawScreen:PROC, _ShuffleCards:PROC, _HitCard:PROC, _RecomputePlayer:PROC, _InitPlayer:PROC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RedrawScreen - prekresli celou obrazovku
;;
;; parametry: zadne
;; navratova hodnota: zadna
;; modifikuje: AX, BX, CX, DX, SI, DI, ES, Flags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
RedrawScreen MACRO
     CALL _RedrawScreen
ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ShuffleCards - zamicha karty
;;
;; parametry: zadne
;; navratova hodnota: zadna
;; modifikuje: AX, BX, CX, DX, SI, DI, ES, Flags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ShuffleCards MACRO
     CALL _ShuffleCards
ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HitCard - hrac si vytahne kartu z balicku
;;
;; parametry:
;;         Player:  Hrac, ktery si kartu vytahne
;; navratova hodnota: zadna
;; modifikuje: AL, SI, DI, Flags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
HitCard MACRO Player
     PUSH OFFSET Player
     CALL _HitCard
ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RecomputePlayer - prepocita karetni soucet hrace
;;
;; parametry:
;;         Player:  Hrac, jehoz soucet ma byt prepocitan
;; navratova hodnota: zadna
;; modifikuje: AX, CX, DL, SI, Flags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
RecomputePlayer MACRO Player
     PUSH OFFSET Player
     CALL _RecomputePlayer
ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; InitPlayer - inicializuje hrace pro novou hru
;;
;; parametry:
;;         Player:  Hrac, jenz ma byt inicializovan
;; navratova hodnota: zadna
;; modifikuje: AL, CX, DI, ES, Flags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
InitPlayer MACRO Player
     PUSH OFFSET Player
     CALL _InitPlayer
ENDM
