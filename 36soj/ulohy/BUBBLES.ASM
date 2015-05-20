MODEL TPASCAL
.CODE

LOCALS

.186

PUBLIC BubbleSort

; ********************************************************
; ** BubbleSort                                         **
; **                                                    **
; ** pouziti:                                           **
; **   procedure BubbleSort(var X: Array of Integer);   **
; ********************************************************

BubbleSort PROC NEAR

ARG   Array:FAR PTR WORD, ArraySize:WORD =ArgLen
LOCAL Max:WORD

     CLD                                ; pouzivame LODSW
     PUSH  DS                           ; uschovame pouzite DS
     MOV   DX, ArraySize                ; DX = jeste nutno setridit prvku
     LDS   DI, Array                    ; DS:DI ukazuje na zacatek pole

     TEST  DX, DX                       ; specialni pripad:
     JZ    @@Exit                       ; jednoprvkove pole -> setrideno
@@MainLoop:
     XOR   BL, BL                       ; BL = priznak "dosud neni hotovo"

     MOV   SI, DI                       ; DS:SI prochazi celym polem
     MOV   CX, DX
@@SortingPass:
     LODSW
     CMP   AX, [SI]                     ; je treba vymenit prvky?
     JG    SHORT @@NeedSwap              ; ANO -> @@NeedSwap
     LOOP  @@SortingPass                 ; NE  -> Dalsi
     JMP   SHORT @@Continue             ; Konec pruchodu, pokracujeme

@@NeedSwap:                             ; je potreba vymenit prvky
     XCHG  AX, [SI]                      ; VYMEN!
     MOV   [SI-2], AX                    ; [SI-2]: uz jsem se posunul dale!
     MOV   BL, 1                         ; nastav priznak, ze neni hotovo
     LOOP  @@SortingPass                ; Konec pruchodu, pokracujeme

@@Continue:                             ; konec hlavniho cyklu
     DEC   DX                           ; posledni prvek je jiz urcite OK
     JZ    @@Exit                       ; setrideno!

     TEST  BL, BL                       ; setrideno?
     JNZ   @@MainLoop

@@Exit:                                 ; pole setrideno, koncime
     POP   DS                           ; vrat DS

     LEAVE                              ; stack frame
     RET ArgLen
BubbleSort ENDP

END