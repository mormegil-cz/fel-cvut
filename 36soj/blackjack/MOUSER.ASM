; MOUSER.ASM - modul pro praci s mysi
; (c) 2000 by DragonSoft

; rozhrani a dokumentace rozhrani v 'MOUSER.INC'

INCLUDE Common.inc

.CONST

NoMouseInstalled DB "Neni nainstalovan ovladac mysi.",13,10,"$"

.DATA?

PUBLIC MouseX
PUBLIC MouseY
PUBLIC MouseBtn

MouseX     DW ?
MouseY     DW ?
MouseBtn   DW ?

.CODE

INCLUDE Display.inc

PUBLIC _InitMouse
PUBLIC _GetMouseAction
PUBLIC _EnableCursor
PUBLIC _DisableCursor

_InitMouse PROC NEAR

    MOV  AX, 21h        ; SW init
    INT  33h
    CMP  AX, 21h        ; OK?
    JE   @@OK

    XOR  AX, AX         ; HW init
    INT  33h
    CMP  AX, -1         ; OK?
    JE   @@OK

    ; Ovladac mysi neni nainstalovan
    MOV  AH, 9
    MOV  DX, OFFSET NoMouseInstalled
    INT  21h

    EXITCODE 1

@@OK:
    RET

ENDP

_EnableCursor PROC NEAR
    MOV  AX, 1
    INT  33h
    RET
ENDP

_DisableCursor PROC NEAR
    MOV  AX, 2
    INT  33h
    RET
ENDP

_GetMouseAction PROC NEAR
@@Repeat:
      MOV  AH, 0Bh              ; stisknuta klavesa?
      INT  21h
      TEST AL, AL
      JZ   @@NoKeyboard
      MOV  AH, 07h              ; ano=>precist
      INT  21h
      CMP  AL, 1Bh              ; ESC?
      JNE  @@NoKeyboard

      ; UKONCI PROGRAM
      CloseDisplay
      EXITCODE 0

@@NoKeyboard:

      MOV  AX, 3                ; dotaz na stav mysi
      INT  33h

      TEST BX, BX               ; stisknuto mysitko?
      JZ   @@Repeat

      MOV  MouseX, CX           ; ano=>vrat pozici a stav
      MOV  MouseY, DX
      MOV  MouseBtn, BX

@@WaitBtnUp:                    ; pockej na uvolneni tlacitka
      MOV  AX, 3                ; dotaz na stav mysi
      INT  33h
      TEST BX, BX               ; stisknuto mysitko?
      JNZ  @@WaitBtnUp

      RET
ENDP

END