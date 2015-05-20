; RANDOM.ASM - modul s generatorem nahodnych cisel
; (c) 2000 by DragonSoft

; rozhrani a dokumentace rozhrani v 'RANDOM.INC'

INCLUDE Common.inc

.CONST

RANDGEN_PRIME           EQU 33797

.DATA?

RandSeed DD ?

.CODE

PUBLIC _Random
PUBLIC _Randomize

_Randomize PROC NEAR
        MOV  AH, 2Ch                    ; zjisti systemove hodiny
        INT  21h
        MOV  WORD PTR [RandSeed], CX    ; hodina, minuta
        MOV  WORD PTR [RandSeed+2], DX  ; sekunda, setina sekundy
        RET
ENDP

RandomStep PROC NEAR
	MOV  AX, WORD PTR [RandSeed]    ; linearni kongruantialni generator
	MOV  BX, WORD PTR [RandSeed+2]
	MOV  CX, AX
        MOV  DX, RANDGEN_PRIME
	MUL  DX
        SHL  CX, 3
	ADD  CH, CL
	ADD  DX, CX
	ADD  DX, BX
        SHL  BX, 2
	ADD  DX, BX
	ADD  DH, BL
        SHL  BX, 5
	ADD  DH, BL
        INC  AX
	ADC  DX, 00h
	MOV  WORD PTR [RandSeed], AX
	MOV  WORD PTR [RandSeed+2], DX
	RET
ENDP

_Random PROC NEAR
ARG Range:WORD = ArgLen

       ENTER 0, 0

       CALL RandomStep
       MOV  AX, DX                      ; zjednodusene oriznuti do rozsahu
       XOR  DX, DX
       DIV  Range
       XCHG AX, DX

       LEAVE
       RET  ArgLen

ENDP

END