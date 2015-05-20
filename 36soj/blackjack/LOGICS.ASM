; LOGICS.ASM - modul pro logiku hry
; (c) 2000 by DragonSoft

; rozhrani a dokumentace rozhrani v 'LOGICS.INC'

INCLUDE Common.inc

.CONST

MAX_PLAYER_CARDS        EQU 10           ; 4x2 + 4x3 + 1xA + (BUST)
MAX_PLAYERS             EQU 10           ; ..?

CARD_COLORS             EQU 4            ; 4 barvy
CARD_VALUES             EQU 13           ; 13 hodnot

CARD_DECK_COUNT         EQU 1                        ; 1-balickova hra
CARD_DECK_SIZE          EQU CARD_COLORS*CARD_VALUES  ; karet v balicku

CARD_COUNT              EQU CARD_DECK_COUNT*CARD_DECK_SIZE  ; karet celkem
BLACK_JACK_COUNT        EQU 21                       ; cil hry

.DATA?

TPlayer STRUC
             Sum       DB ?
             CardCount DW ?
             Cards     DB MAX_PLAYER_CARDS DUP (?)
ENDS

Bet    DW ?                             ; vyse sazky

Player TPlayer ?                        ; hrac
Dealer TPlayer ?                        ; banker

ShowDealerCards DB ?                    ; zobrazovat karty bankere?

CardDeck DB CARD_COUNT DUP (?)

PUBLIC Bet, Player, Dealer, ShowDealerCards

.DATA

Cash     DW 100                         ; hotovost hrace
DeckPos  DW CARD_COUNT                  ; pozice v balicku

PUBLIC DeckPos, Cash

.CODE

PUBLIC _RedrawScreen, _ShuffleCards, _HitCard, _RecomputePlayer, _InitPlayer

INCLUDE Cards.inc
INCLUDE Display.inc
INCLUDE Random.inc

_RedrawScreen PROC NEAR

     ClrScr

     PaintReverse DECK_CARDS_X, DECK_CARDS_Y

     ; vykresli karty hrace
     MOV  DX, PLAYER_CARDS_X
     MOV  CX, Player.CardCount
     XOR  BX, BX
@@NextCardP:
     PUSH DX
     PUSH CX
     PUSH BX

     MOV  AL, Player.Cards[BX]
     PaintCard DX, PLAYER_CARDS_Y, AX

     POP  BX
     POP  CX
     POP  DX

     INC  BX                    ; Dalsi karta
     ADD  DX, PLAYER_CARDS_DX

     LOOP @@NextCardP

     MOV  TextColor, White
     MOV  AL, Player.Sum
     XOR  AH, AH
     IntOut PLAYER_CARDS_X, PLAYER_CARDS_Y+65, AX

     ; vykresli karty bankere
     MOV  DX, DEALER_CARDS_X
     MOV  CX, Dealer.CardCount
     XOR  BX, BX
@@NextCardD:
     PUSH DX
     PUSH CX
     PUSH BX

     CMP  ShowDealerCards, 0            ; vykreslovat karty bankere?
     JZ   @@OnlyReverse
@@PaintFirst:
     MOV  AL, Dealer.Cards[BX]          ; ano
     PaintCard DX, DEALER_CARDS_Y, AX
     JMP  @@Painted
@@OnlyReverse:
     TEST BX, BX                        ; prvni karta?
     JZ   @@PaintFirst                  ; ano=>vzdy vykresluj
     PaintReverse DX, DEALER_CARDS_Y    ;
@@Painted:

     POP  BX
     POP  CX
     POP  DX

     INC  BX                            ; Dalsi karta
     ADD  DX, PLAYER_CARDS_DX

     LOOP @@NextCardD

     MOV  TextColor, White
     CMP  ShowDealerCards, 0            ; zobrazovat bankeruv soucet?
     JZ   @@SkipDealerSum
     MOV  AL, Dealer.Sum
     XOR  AH, AH
     IntOut DEALER_CARDS_X, DEALER_CARDS_Y+65, AX

@@SkipDealerSum:
     CharOut CASH_X, CASH_Y, "$"
     IntOut CASH_X+8, CASH_Y, Cash      ; zobraz penize hrace
     CharOut CASH_X, CASH_Y+8, "$"
     IntOut CASH_X+8, CASH_Y+8, Bet     ; zobraz vysi sazky

     RET
ENDP

_ShuffleCards PROC NEAR
LOCAL TmpDeck:BYTE:CARD_COUNT = LocalLen

    ENTER LocalLen, 0

    PUSH  SS                            ; TmpDeck je na stacku
    POP   ES
    LEA   DI, TmpDeck
    CLD

    MOV   BX, CARD_DECK_COUNT           ; vytvor CARD_DECK_COUNT novych balicku
@@NextDeck:
    MOV   DX, CARD_VALUES               ; kazdy obsahuje CARD_VALUES hodnot
@@NextValue:
    MOV   AL, DL
    SHL   AL, 4                         ; hodnota v hornich 4 bitech
    MOV   CX, CARD_COLORS               ; kazda hodnota o CARD_COLORS barvach
@@NextColor:
    STOSB
    INC   AL                            ; posun na dalsi barvu (v dolnich 4 bitech)
    LOOP  @@NextColor
    DEC   DX
    JNZ   @@NextValue
    DEC   BX
    JNZ   @@NextDeck

    MOV   CX, CARD_COUNT
@@NextCard:
    PUSH CX                             ; vezmi jeste nezamichanou kartu
    Random CX
    POP  CX
    MOV  SI, AX
    MOV  DI, CX
    DEC  DI

    MOV  AL, TmpDeck[SI]                ; pridej ji do balicku
    MOV  CardDeck[DI], AL
    MOV  AL, TmpDeck[DI]                ; a odstran ji ze seznamu nezamichanych
    MOV  TmpDeck[SI], AL

    LOOP @@NextCard

    MOV  DeckPos, 0

    LEAVE
    RET
ENDP

_HitCard PROC NEAR
ARG Pl:NEAR PTR TPlayer = ArgLen

     ENTER 0, 0

     MOV   SI, DeckPos
     MOV   DI, Pl
     PUSH  DI
     ADD   DI, OFFSET TPlayer.CardCount
     MOV   DI, [DI]
     ADD   DI, Pl
     MOV   AL, CardDeck[SI]             ; karta ze zacatku balicku
     MOV   DI[TPlayer.Cards], AL        ; na konec hracovych karet
     POP   DI

     INC   DI[TPlayer.CardCount]        ; hrac ma o kartu vic
     INC   DeckPos                      ; a v balicku je o kartu min

     CMP   DeckPos, CARD_COUNT          ; pokud je balicek vypotrebovan,
     JB    @@Enough
     CALL  _ShuffleCards                ; zamicha se novy
@@Enough:

     LEAVE
     RET  ArgLen

ENDP

_RecomputePlayer PROC NEAR
ARG Pl:NEAR PTR TPlayer = ArgLen

     ENTER 0, 0

     ; nejprve soucet bez es
     MOV   SI, Pl
     MOV   CX, SI[TPlayer.CardCount]
     ADD   SI, OFFSET TPlayer.Cards
     XOR   DL, DL                       ; DL...soucet
@@NextCard:
     LODSB
     SHR   AL, 4                        ; chci pouze hodnotu
     CMP   AL, cardAce                  ; eso?
     JE    @@SkipAce

     ADD  DL, AL
     CMP  AL, card10                    ; obrazkova karta?
     JB   @@NumberCard                  ; ne=>hodnota=cislo
     SUB  DL, AL                        ; ano=>hodnota=10
     ADD  DL, 10
@@NumberCard:
@@SkipAce:
     LOOP  @@NextCard

     ; a ted pridat esa
     MOV   SI, Pl
     MOV   CX, SI[TPlayer.CardCount]
     ADD   SI, OFFSET TPlayer.Cards
@@NextCard2:
     LODSB
     SHR   AL, 4                        ; chci pouze hodnotu
     CMP   AL, cardAce                  ; eso?
     JNE   @@SkipNoAce

     CMP   DL, BLACK_JACK_COUNT-11      ; rozlis tvrde/mekke eso
     JA    @@HardAce
     ADD   DL, 10                       ; mekke = 10+1
@@HardAce:
     INC   DL                           ; tvrde = 1
@@SkipNoAce:
     LOOP  @@NextCard2

     MOV   SI, Pl                       ; uloz soucet
     MOV   SI[TPlayer.Sum], DL

     LEAVE
     RET  ArgLen

ENDP

_InitPlayer PROC NEAR
ARG Pl:NEAR PTR TPlayer = ArgLen

     ENTER 0, 0

     CLD                                ; vynuluj hrace
     PUSH  DS
     POP   ES
     MOV   DI, Pl
     MOV   CX, SIZE TPlayer
     XOR   AL, AL
     REP   STOSB

     LEAVE
     RET  ArgLen

ENDP

END