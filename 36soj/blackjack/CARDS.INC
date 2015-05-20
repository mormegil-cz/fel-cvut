; CARDS.INC - modul pro praci s kartami - rozhrani
; (c) 2000 by DragonSoft

EXTRN _InitCards:PROC, _PaintCard:PROC, _PaintReverse:PROC
; vlastni kod je v 'CARDS.ASM'

; Karetni barvy
colorHearts   EQU 0
colorClubs    EQU 1
colorDiamonds EQU 2
colorSpades   EQU 3

; Karetni hodnoty
cardAce       EQU 1
card2         EQU 2
card3         EQU 3
card4         EQU 4
card5         EQU 5
card6         EQU 6
card7         EQU 7
card8         EQU 8
card9         EQU 9
card10        EQU 10
cardJack      EQU 11
cardQueen     EQU 12
cardKing      EQU 13

; rozmisteni karet na obrazovce (souradnice)
DECK_CARDS_X   EQU 5
DECK_CARDS_Y   EQU 5
PLAYER_CARDS_X EQU 5
PLAYER_CARDS_Y EQU 100
DEALER_CARDS_X EQU 50
DEALER_CARDS_Y EQU 5

CASH_X          EQU 280
CASH_Y          EQU 5

PLAYER_CARDS_DX EQU 15

PLAYERS_DX      EQU 30

STANDZONE_X     EQU DECK_CARDS_X+40
STANDZONE_Y     EQU DECK_CARDS_Y+60
HITZONE_X       EQU PLAYER_CARDS_X+2*PLAYER_CARDS_DX+40
HITZONE_Y       EQU PLAYER_CARDS_Y
DBLZONE_X       EQU CASH_X-2
DBLZONE_Y       EQU CASH_Y+16+2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; InitCards - inicializuje karetni modul
;;
;; parametry: zadne
;; navratova hodnota: zadna
;; pred zavolanim musi byt inicializovana grafika (InitDisplay)
;;
;; modifikuje: AX, BX, CX, DX, SI, DI, Flags, ES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
InitCards MACRO
          CALL _InitCards
ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PaintCard - vykresli kartu
;;
;; parametry:
;;       X, Y:  souradnice leveho horniho rohu karty k vykresleni
;;       Card:  karta, ktera se ma vykreslit
;;
;; navratova hodnota: zadna
;;
;; modifikuje: AX, BX, CX, DX, SI, DI, Flags, ES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PaintCard MACRO X, Y, Card
          PUSH Card
          PUSH Y
          PUSH X
          CALL _PaintCard
ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PaintReverse - vykresli rub karty
;;
;; parametry:
;;       X, Y:  souradnice leveho horniho rohu karty k vykresleni
;;
;; navratova hodnota: zadna
;;
;; modifikuje: AX, BX, CX, DX, SI, DI, Flags, ES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PaintReverse MACRO X, Y
             PUSH Y
             PUSH X
             CALL _PaintReverse
ENDM
