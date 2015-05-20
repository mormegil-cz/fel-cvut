; BLJACK.ASM - hlavni modul hry Black Jack
; (c) 2000 by DragonSoft

INCLUDE Common.inc

.STACK 8192

.CONST

ANTE_BET EQU 2

Win_Text       DB "Zvitezil jste!",0
Draw_Text      DB "Hra skoncila nerozhodne",0
Lost_Text      DB "Prohral jste",0
Wait_Click     DB "klikni...",0
Insurance_Text DB "Chcete se pojistit?",0
CashLost_Text  DB "Prohral jste vsechny uspory...",0

;Beep_Str       DB 7,"$"       ; retezec pro pipnuti

DELAY_LENGTH EQU 15           ; Delka cekani mezi dealerovymi kartami (x 55ms)

.CODE

INCLUDE Display.inc
INCLUDE TGA.inc
INCLUDE Cards.inc
INCLUDE Mouser.inc
INCLUDE Logics.inc
INCLUDE Random.inc

Delay MACRO             ; vyckej
LOCAL @@NextTick, @@Wait

      MOV  CX, DELAY_LENGTH
      XOR  AX, AX
      MOV  ES, AX
      MOV  SI, 046Ch     ; BIOS tick count
@@NextTick:
      MOV  AX, ES:[SI]
@@Wait:
      CMP  AX, ES:[SI]
      JE   @@Wait
      LOOP @@NextTick

ENDM

ErrorBeep MACRO
     PUSH AX
     PUSH DX
     MOV  AH, 2
     MOV  DL, 7
     INT  21h
     POP  DX
     POP  AX
ENDM

_Main PROC NEAR
LOCAL Win:WORD,Insured:BYTE,Fill:BYTE = LocalLen

          ENTER LocalLen, 0

          InitMouse
          InitDisplay
          InitCards
          Randomize

@@NextGame:
          MOV  Bet, ANTE_BET            ; uvodni sazka
          CMP  DeckPos, CARD_COUNT-4    ; jsou v balicku jeste karty?
          JBE  @@EnoughStack
          ShuffleCards                  ; ne=>zamichat
@@EnoughStack:
          InitPlayer Player             ; inicializace hracu
          InitPlayer Dealer

          MOV  ShowDealerCards, 0       ; nezobrazovat karty bankere

          HitCard Player                ; rozdani
          HitCard Dealer
          HitCard Player
          HitCard Dealer

          RecomputePlayer Player
          RecomputePlayer Dealer

          RedrawScreen

          ; pojisteni
          MOV  Insured, 0                       ; defaultne bez pojisteni

          MOV  AX, Bet                          ; ma vubec dost penez na pojisteni?
          SHR  AX, 1
          CMP  Cash, AX
          JB   @@SkipInsurance

          MOV  AL, Dealer.Cards[0]              ; je prvni krupierova karta eso?
          SHR  AL, 4
          CMP  AL, cardAce
          JNE  @@SkipInsurance
          MOV  TextColor, White
          TextOut 5, 190, Insurance_Text
@@GetInput:
          MOV  AH, 07h                          ; vstup znaku
          INT  21h
          CMP  AL, "A"
          JE   @@MakeInsurance
          CMP  AL, "a"
          JE   @@MakeInsurance
          CMP  AL, "N"
          JE   @@NoInsurance
          CMP  AL, "n"
          JE   @@NoInsurance
          JMP  @@GetInput
@@MakeInsurance:
          MOV  Insured, 1
@@NoInsurance:
          RedrawScreen
@@SkipInsurance:
          ; kontrola BlackJacku
          CMP  Player.Sum, BLACK_JACK_COUNT
          JNE  @@NoPlayerBJ
          CMP  Dealer.Sum, BLACK_JACK_COUNT
          JE   @@DrawGame                       ; oba hraci maji blackjack?
          ; ne=>hrac vitezi na blackjack
          MOV  AX, Bet                          ; vyhra = 1.5*sazka
          MOV  BX, AX
          SHL  AX, 1
          ADD  AX, BX                           ; 3*sazka
          SHR  AX, 1                            ; 3*sazka/2
          MOV  Win, AX
          JMP  @@WonGame
@@NoPlayerBJ:
          CMP  Dealer.Sum, BLACK_JACK_COUNT
          JNE  @@NoneBJ
          ; dealer ma blackjack
          CMP  Insured, 0
          JZ   @@LostGame
          JMP  @@DrawGame
@@NoneBJ: ; bez blackjacku
          CMP  Insured, 0                       ; propadnuti pojisteni?
          JZ   @@PlayerPlay
          MOV  AX, Bet
          SHR  AX, 1                            ; 1/2 puvodni sazky
          SUB  Cash, AX
          RedrawScreen

@@PlayerPlay:
          EnableCursor                          ; vstup pokynu od hrace
          GetMouseAction
          DisableCursor

          MOV  AX, MouseY
          MOV  BX, MouseX
          CMP  AX, STANDZONE_Y
          JA   @@DontStand
          CMP  BX, STANDZONE_X
          JA   @@DontStand

          CMP  Player.Sum, MIN_STAND_SUM        ; STAND
          JAE  @@PlayerFinished
          ErrorBeep
          JMP  @@DontStand

@@DontStand:
          CMP  AX, HITZONE_Y
          JB   @@DontHit
          CMP  BX, HITZONE_X
          JA   @@DontHit

          HitCard Player                        ; HIT
          RecomputePlayer Player
          RedrawScreen

          CMP  Player.Sum, BLACK_JACK_COUNT
          JAE  @@PlayerFinished
          JMP  @@PlayerPlay
@@DontHit:
          CMP  AX, DBLZONE_Y
          JA   @@DontDouble
          CMP  BX, DBLZONE_X
          JB   @@DontDouble

          MOV  AX, Bet                          ; DOUBLE

          CMP  Cash, AX                         ; ma dost penez na zdvojnasobeni?
          JAE  @@CanDouble

          ErrorBeep
          JMP  @@DontDouble

@@CanDouble:
          SHL  AX, 1
          MOV  Bet, AX
          HitCard Player
          RecomputePlayer Player
          RedrawScreen
          JMP  @@PlayerFinished
@@DontDouble:
          JMP  @@PlayerPlay
@@PlayerFinished:
          CMP  Player.Sum, BLACK_JACK_COUNT     ; Prekroceno?
          JA   @@LostGame

          MOV  ShowDealerCards, 1               ; zobrazovat bankerovy karty
          RedrawScreen

@@DealerPlay:                                   ; hraje banker
          CMP  Dealer.Sum, DEALER_STAND_SUM
          JAE  @@DealerFinished

          Delay                                 ; pauzicka

          HitCard Dealer
          RecomputePlayer Dealer
          RedrawScreen

          JMP  @@DealerPlay
@@DealerFinished:
          ; hra skoncena, kdo vyhral?
          MOV  AL, Dealer.Sum
          CMP  AL, BLACK_JACK_COUNT
          JA   @@DealerBust                     ; banker prekrocil?

          CMP  AL, Player.Sum                   ; ne: kdo ma vic?
          JA   @@LostGame                       ; banker
          JE   @@DrawGame                       ; remiza
@@DealerBust:
          MOV  AX, Bet                          ; zvitezil hrac
          MOV  Win, AX                          ; vitezna castka = sazka
@@WonGame:
          MOV  AX, Win                          ; pridat vyhru
          ADD  Cash, AX

          MOV  ShowDealerCards, 1               ; zobrazovat bankerovy karty
          RedrawScreen

          MOV  TextColor, White
          TextOut 5, 190, Win_Text
          JMP  @@EndGame
@@LostGame:
          MOV  ShowDealerCards, 1               ; zobrazovat bankerovy karty
          RedrawScreen

          MOV  TextColor, White
          TextOut 5, 190, Lost_Text

          MOV  AX, Bet                          ; prohra = sazka
          SUB  Cash, AX
          JC   @@CashLost                       ; jeste mam penize?
          CMP  Cash, ANTE_BET
          JB   @@CashLost

          JMP  @@EndGame
@@DrawGame:
          MOV  ShowDealerCards, 1               ; zobrazovat bankerovy karty
          RedrawScreen

          MOV  TextColor, White
          TextOut 5, 190, Draw_Text
@@EndGame:

          TextOut 240, 190, Wait_Click
          GetMouseAction                        ; pockej na kliknuti

          JMP  @@NextGame

          ; prohral vsechny penize...
@@CashLost:
          ClrScr

          MOV  TextColor, Red
          TextOut 10, 100, CashLost_Text

          MOV  TextColor, White
          TextOut 240, 190, Wait_Click
          GetMouseAction                        ; pockej na kliknuti

          CloseDisplay
          LEAVE
          RET
ENDP

; --- program entry point:
@@Start:  STARTUPCODE
          CALL _Main
          EXITCODE 0

END