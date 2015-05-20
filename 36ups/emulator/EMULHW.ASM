.MODEL TPASCAL
.CODE
.186
JUMPS

COMMENT #

        To fix (?): if two interrupts are just one after another, the second
               one is possibly not emulated (?) - BreakISR just restore the
               0CDh opcode.

#

ASSUME CS:CODE, DS:CODE

PUBLIC StepISR, BreakISR, EmulHWData

; -------------------
; IsRisingEdge macro:
; test if the bit specified by its bitmask has rising edge
; if yes, set CF, otherwise clear it
; Assumptions: BL contains data currently being sent to port
; Modifies: AL, CL
; -------------------
IsRisingEdge MACRO mask
LOCAL NotRising, Exit

        MOV  CL, mask
        MOV  AL, CurrState
        AND  AL, CL
        JNZ  SHORT NotRising
        MOV  AL, BL
        AND  AL, CL
        JZ   SHORT NotRising
        STC
        JMP  Exit
NotRising:
        CLC
Exit:

ENDM

; -------------------
; StoreDate macro:
; store something to cyclic buffer (FOR DEBUGGING PURPOSES ONLY)
; Modifies: only cyclic buffer variables
; -------------------
StoreData MACRO something

        PUSH SI
        PUSH AX
        MOV  AL, something
        MOV  SI, OFFSET Buffer
        ADD  SI, BuffPos
        MOV  [SI], AL
        INC  BYTE PTR BuffPos
        POP  AX
        POP  SI
ENDM

; --- constants ---
opcOUT_DX_AL    EQU 0EEh
opcIN_AL_DX     EQU 0ECh
opcNOP          EQU 090h
opcINTxx        EQU 0CDh
opcINT3         EQU 0CCh

OutputPort      EQU 378h
InputPort       EQU 379h

; -------- Data area ----------

EmulHWData LABEL

RestoreCode  DB 0
RestoreCS    DW ?
RestoreIP    DW ?

RestoreBreak DB ?
BreakSet     DB 0

DoExitProg   DB 0
StopTracing  DB 0

CurrState    DB 0

SwitchState  DB 7Fh
ButtonState  DB 7Fh

VehicleState DW 0FFFFh
PedestrState DB 0FFh
LEDState     DB 0FFh

; ------- Error counts --------
eUnselectedInput DD 0
eOutputToInput   DD 0
eInputFromOutput DD 0
; -----------------------------

;TstData      DW 0
;BuffPos      DW 0
;Buffer       DB 256 DUP (0)

StepISR PROC FAR

        PUSHA
        PUSH DS
        PUSH ES

        MOV  BP, SP

        MOV  AX, CS
        MOV  DS, AX

        CLD

        MOV  AX, [BP+16h]       ; stored CS
        MOV  ES, AX
        MOV  DI, [BP+14h]       ; stored IP

        ; if DoExitProg<>0 then begin TerminateProgram; Exit; end;
        CMP  DoExitProg, 0
        JZ   SHORT DoNotTerminate

        MOV  SI, OFFSET TerminationCode
        MOV  CX, TermCodeLen
        REP  MOVSB
        MOV  DoExitProg, 0
        MOV  StopTracing, 1     ; program will be terminated - do not trace any more
        JMP  Exit
DoNotTerminate:

        ; if RestoreCode<>0 then restore;
        MOV  AL, RestoreCode
        TEST AL, AL
        JZ   SHORT NothingToRestore
                               ; restore!
        PUSH ES
        PUSH DI

        MOV  DI, RestoreCS
        MOV  ES, DI
        MOV  DI, RestoreIP

        STOSB

        POP  DI
        POP  ES

        MOV  RestoreCode, 0
NothingToRestore:

        MOV  AL, BYTE PTR ES:[DI]               ; AL = next instruction

        ; if NextInstr="in al, dx" then goto emulIN_AL_DX
        CMP  AL, opcIN_AL_DX
        JE   SHORT emulIN_AL_DX
        ; else if NextInstr="out dx, al" then goto emulOUT_DX_AL
        CMP  AL, opcOUT_DX_AL
        JE   emulOUT_DX_AL   ; <<<<<<*****#### insert SHORT
        ; else if NextInstr<>"int xx" then exit;
        CMP  AL, opcINTxx
        JNE  Exit

        ; --- INT xx - make sure tracing won't be switched off
        CMP  StopTracing, 0
        JNZ  Exit                              ; No tracing - exit
        CMP  BYTE PTR ES:[DI+1], 21h           ; Is that INT 21h ?
        ;JNE  NotINT21
        ;CMP  BYTE PTR [BP+13h], 4Ch            ; Is Stored AH=4C ?
        JE   Exit                              ; Yes => ignore this INT
NotINT21:
        MOV  AL, ES:[DI+2]                     ; Store original instruction
        MOV  RestoreBreak, AL
        MOV  BreakSet, 1
        MOV  BYTE PTR ES:[DI+2], opcINT3       ; Set breakpoint
        JMP  Exit

emulIN_AL_DX:
        ; --- emulate "in al, dx"
        ; if DX<>InputPort then Exit;
        CMP  DX, InputPort
        JE   INfromInput
        CMP  DX, OutputPort
        JNE  Exit
        MOV  BX, OFFSET eInputFromOutput
        JMP  Error
INfromInput:
        ; Test if input selected (buttons/switches)
        MOV  BX, OFFSET eUnselectedInput
        MOV  AL, CurrState
        AND  AL, 5
        JZ   Error
        CMP  AL, 5
        JE   Error

        MOV  RestoreCS, ES
        MOV  RestoreIP, DI
        MOV  RestoreCode, opcIN_AL_DX           ; Save current instruction
        MOV  BYTE PTR ES:[DI], opcNOP           ; Spoof the instruction

        ; emulate the instruction
        TEST CurrState, 1                       ; if CurrState.D0=0 then AL:=ButtonState
        JZ   SHORT InputSwitches                ;                   else AL:=SwitchState
        MOV  AL, SwitchState
        JMP  SHORT FakeInput
InputSwitches:
        MOV  AL, ButtonState
FakeInput:
        MOV  [BP+12h], AL                        ; stored AL
        JMP  Exit

emulOUT_DX_AL:
        ; --- emulate "out dx, al"
        ; if DX<>OutputPort then Exit;

        CMP  DX, OutputPort
        JE   SHORT OUTtoOutput
        CMP  DX, InputPort
        JNE  Exit
        MOV  BX, OFFSET eOutputToInput
        JMP  Error
OUTtoOutput:
        MOV  RestoreCS, ES
        MOV  RestoreIP, DI
        MOV  RestoreCode, opcOUT_DX_AL          ; Save current instruction
        MOV  BYTE PTR ES:[DI], opcNOP           ; Spoof the instruction

        ; emulate the instruction
        MOV  BL, [BP+12h]                       ; stored AL

        PUSH 0A000h                             ; Prepare videosegment
        POP  ES

        ; if RisingEdge(D1) then StoreV
        IsRisingEdge 02h
        JNC  SHORT NotRisingV
        ; StoreV

        MOV  AX, VehicleState
        SHR  AX, 1
        MOV  CL, BL
        SHL  CX, 15
        OR   AX, CX
        MOV  VehicleState, AX

        ; ******* Redraw vehicle state *******
        MOV  CX, AX                     ; CX=Vehicle state

        MOV  SI, OFFSET VSemData        ; SI=Offset to data
        MOV  DX, 8000h                  ; DX=Bit mask
NextVLight:
        MOV  AX, [SI+2]                 ; AX=Color (HI byte=LO byte)
        MOV  DI, [SI]                   ; DI=Location on screen
        TEST CX, DX
        JNZ  SHORT VLightIsOff
        ADD  AX, 0808h                  ; If lit, increase color brightness
VLightIsOff:
        STOSB
        ADD  DI, 318
        STOSW
        STOSB
        ADD  DI, 318
        STOSB

        TEST CX, DX
        JNZ  SHORT VLightIsOff2
        ADD  AX, 0808h
        MOV  BYTE PTR ES:[DI-321], AL

VLightIsOff2:

        ADD  SI, 4                      ; Move in data to the next light
        SHR  DX, 1                      ; Shift mask down by one
        JNZ  NextVLight                 ; If not finished, do next light

NotRisingV:
        ; if RisingEdge(D3) then StoreP
        IsRisingEdge 08h
        JNC  SHORT NotRisingP
        ; StoreP
        MOV  AL, PedestrState
        SHR  AL, 1
        MOV  CL, BL
        AND  CL, 04h
        SHL  CL, 5
        OR   AL, CL
        MOV  PedestrState, AL

        ; ******* Redraw pedestrian state *******
        MOV  CL, AL                     ; CL=Pedestrian state

        MOV  SI, OFFSET PSemData        ; SI=Offset to data
        MOV  DL, 80h                    ; DX=Bit mask
NextPLight:
        MOV  AX, [SI+2]                 ; AX=Color (HI byte=LO byte)
        MOV  DI, [SI]                   ; DI=Location on screen
        TEST CL, DL
        JNZ  SHORT PLightIsOff
        ADD  AX, 0808h                  ; If lit, increase color brightness
PLightIsOff:
        STOSB
        ADD  DI, 318
        STOSW
        STOSB
        ADD  DI, 318
        STOSB

        TEST CX, DX
        JNZ  SHORT PLightIsOff2
        ADD  AX, 0808h
        MOV  BYTE PTR ES:[DI-321], AL

PLightIsOff2:

        ADD  SI, 4                      ; Move in data to the next light
        SHR  DL, 1                      ; Shift mask down by one
        JNZ  NextPLight                 ; If not finished, do next light

NotRisingP:
        ; if RisingEdge(D5) then StoreL
        IsRisingEdge 20h
        JNC  SHORT NotRisingL
        ; StoreL
        MOV  AL, LEDState
        SHR  AL, 1
        MOV  CL, BL
        AND  CL, 10h
        SHR  CL, 1
        AND  CL, 08h
        OR   AL, CL
        MOV  LEDState, AL

        ; ******* Redraw LED state *******
        MOV  CL, AL                     ; CL=LED state

        MOV  SI, OFFSET LSemData        ; SI=Offset to data
        MOV  DL, 08h                    ; DX=Bit mask
NextLLight:
        MOV  AX, [SI+2]                 ; AX=Color (HI byte=LO byte)
        MOV  DI, [SI]                   ; DI=Location on screen
        TEST CL, DL
        JNZ  SHORT LLightIsOff
        ADD  AX, 0808h                  ; If lit, increase color brightness
LLightIsOff:
        STOSB
        ADD  DI, 318
        STOSW
        STOSB
        ADD  DI, 318
        STOSB

        TEST CX, DX
        JNZ  SHORT LLightIsOff2
        ADD  AX, 0808h
        MOV  BYTE PTR ES:[DI-321], AL

LLightIsOff2:

        ADD  SI, 4                      ; Move in data to the next light
        SHR  DL, 1                      ; Shift mask down by one
        JNZ  NextLLight                 ; If not finished, do next light
NotRisingL:
        MOV  CurrState, BL

        JMP  SHORT Exit

Error:  ; --- Error ---
        ; BX containts offset to error counter
        INC  WORD PTR [BX]
        ADC  WORD PTR [BX+2], 0
Exit:   ; --- Finish ---

                                        ; Ensure tracing is still running
        MOV  AX, [BP+18h]               ; AX:=Original flags
        AND  AX, 0FEFFh                 ; Clear TF:=0 (stop tracing)
        CMP  CS:StopTracing, 0
        JNZ  SHORT ClearTF
        OR   AX, 0100h                  ; Set TF:=1
ClearTF:
        MOV  [BP+18h], AX               ; Store the modified flags back to stack

        POP  ES
        POP  DS
        POPA
        IRET

ENDP

BreakISR PROC FAR                       ; Breakpoint ISR code

        PUSH AX
        PUSH BX
        PUSH BP
        PUSH DS
        PUSH ES

        MOV  BP, SP

        MOV  AX, CS
        MOV  DS, AX

        CMP  BreakSet, 0                ; Breakpoint not set by me
        JZ   ExitBreak

        DEC  WORD PTR [BP+0Ah]          ; Decrease IP so that the original instruction is executed

        MOV  AX, [BP+0Ch]               ; Stored CS
        MOV  ES, AX
        MOV  BX, [BP+0Ah]               ; Stored IP

        MOV  BreakSet, 0                ; Breakpoint cleared
        MOV  AL, RestoreBreak
        MOV  ES:[BX], AL                ; Restore the original instruction

                                        ; Ensure tracing is still running
        MOV  AX, [BP+0Eh]               ; AX:=Original flags
        AND  AX, 0FEFFh                 ; Clear TF:=0 (stop tracing)
        CMP  StopTracing, 0
        JNZ  SHORT BrkClearTF
        OR   AX, 0100h                  ; Set TF:=1
BrkClearTF:
        MOV  [BP+0Eh], AX               ; Store the modified flags back to stack

ExitBreak:
        POP  ES
        POP  DS
        POP  BP
        POP  BX
        POP  AX
        IRET

ENDP

          ; ---- Data area ----

; Code for program termination
TermCodeLen     EQU 5
TerminationCode DB  0B8h, 000h, 04Ch, 0CDh, 021h

SEM_X_1  EQU 76
SEM_X_2  EQU 124

SEM_Y_1  EQU 75
SEM_Y_2  EQU 123

VSemData: ; Vehicle semaphors

        ; West
        DW  SEM_Y_2*320+SEM_X_1        ; +0       Screen position
        DW  0404h                      ; +2       Color (LO byte = HIGH byte)

        DW  SEM_Y_2*320+SEM_X_1-5
        DW  0303h

        DW  SEM_Y_2*320+SEM_X_1-10
        DW  0202h

        DW  (SEM_Y_2+5)*320+SEM_X_1
        DW  0202h

        ; East
        DW  SEM_Y_1*320+SEM_X_2
        DW  0404h

        DW  SEM_Y_1*320+SEM_X_2+5
        DW  0303h

        DW  SEM_Y_1*320+SEM_X_2+10
        DW  0202h

        DW  (SEM_Y_1-5)*320+SEM_X_2
        DW  0202h

        ; North
        DW  SEM_Y_1*320+SEM_X_1
        DW  0404h

        DW  (SEM_Y_1-5)*320+SEM_X_1
        DW  0303h

        DW  (SEM_Y_1-10)*320+SEM_X_1
        DW  0202h

        DW  SEM_Y_1*320+SEM_X_1-5
        DW  0202h

        ; South
        DW  SEM_Y_2*320+SEM_X_2
        DW  0404h

        DW  (SEM_Y_2+5)*320+SEM_X_2
        DW  0303h

        DW  (SEM_Y_2+10)*320+SEM_X_2
        DW  0202h

        DW  SEM_Y_2*320+SEM_X_2+5
        DW  0202h

PSemData: ; Pedestrian semaphors
        DW  SEM_Y_1*320+50
        DW  0404h

        DW  (SEM_Y_1-5)*320+50
        DW  0202h

        DW  SEM_Y_2*320+150
        DW  0404h

        DW  (SEM_Y_2+5)*320+150
        DW  0202h

        DW  50*320+SEM_X_2
        DW  0404h

        DW  50*320+SEM_X_2+5
        DW  0202h

        DW  150*320+SEM_X_1
        DW  0404h

        DW  150*320+SEM_X_1-5
        DW  0202h

LSemData: ; LED semaphors

        DW  170*320+10
        DW  0202h

        DW  170*320+20
        DW  0202h

        DW  170*320+30
        DW  0202h

        DW  170*320+40
        DW  0202h

        ; -- End of semaphors data --

END