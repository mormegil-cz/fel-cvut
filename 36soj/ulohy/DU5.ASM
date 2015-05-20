; 5. domaci ukol ze SOJ
; Petr Kadlec, cviceni ze SOJ ve ctvrtek od 9:15

; Zadani:
;     zobrazte stranku souboru na obrazovce a pri pohybu kurzoru zvyraznujte
;     radek, na kterem se kurzor nachazi

.MODEL SMALL

LOCALS
.286

.STACK

.DATA?

MaxRow   DB ?                           ; celkovy pocet radek

FileData DB 80*25 DUP (?)               ; zobrazeny soubor
FileLen  DW ?                           ; delka souboru

.CONST

FileName DB "DU5.TXT",0
ErrorMsg DB "Nelze otevrit soubor DU5.TXT",13,10,"$"

.CODE

ReadFile PROC NEAR

        MOV  AX, 03D00h                 ; otevri soubor, read-only
        MOV  DX, OFFSET FileName
        INT  21h
        JC   @@Error
        MOV  BX, AX                     ; BX = handle

        MOV  AH, 3Fh                    ; precti data
        MOV  DX, OFFSET FileData
        MOV  CX, SIZE FileData
        INT  21h
        MOV  FileLen, CX

        MOV  AH, 3Eh                    ; zavri soubor
        INT  21h
        JMP  @@Exit

@@Error:MOV  DX, OFFSET ErrorMsg
        MOV  AH, 9
        INT  21h
        EXITCODE 1

@@Exit:
        RET
ENDP

DisplayFile PROC NEAR

        MOV  AX, 0600h                  ; smaz obrazovku
        XOR  CX, CX                     ; [0,0] -
        MOV  DX, 184Fh                  ;         [79,24]
        MOV  BH, 7
        INT  10h

        MOV  AH, 2                      ; Nastav pozici kurzoru
        XOR  BH, BH
        XOR  DX, DX                     ; na [0,0]
        INT  10h

        CLD
        MOV  SI, OFFSET FileData        ; SI = ukazatel do dat
        MOV  CX, FileLen                ; CX = zbyvajici delka
        XOR  DI, DI                     ; DI = soucasna radka
        XOR  BX, BX                     ; BX = soucasny sloupec

@@NextChar:
        LODSB
        XCHG AX, DX
        MOV  AH, 02                     ; =zobraz znak
        INT  21h
        CMP  DL, 10                     ; konec radky?
        JNE  @@NoLF
@@EOLn:
        XOR  BX, BX
        INC  DI
        CMP  DI, 24                     ; konec obrazovky?
        JAE  @@Exit
        LOOP @@NextChar
        JMP  @@Exit
@@NoLF: INC  BX
        CMP  BX, 80                     ; konec radky?
        JAE  @@EOLn
        LOOP @@NextChar
@@Exit:
        CMP  BX, 0                      ; je posledni radek pouzit?
        JNE  @@NotEmpty
        DEC  DI                         ; ano=>na nej uz nesmime
@@NotEmpty:
        MOV  AX, DI
        MOV  MaxRow, AL                 ; posledni radka

        RET
ENDP

Highlight PROC NEAR                     ; nastavi atribut radku na AL

       PUSH 0B800h
       POP  ES

       MOV  BX, DX
       SHL  BX, 5                       ; DX*32
       MOV  DI, BX
       SHL  BX, 2                       ; DX*32*4=DX*128
       ADD  DI, BX                      ; DX*32+DX*32*4 = DX*160

       MOV  CX, 80
       CLD
@@NextChar:
       INC  DI
       STOSB
       LOOP @@NextChar

       PUSH DX
       MOV  AH, 2                       ; nastav pozici kurzoru na [0,DX]
       XOR  BH, BH
       MOV  DH, DL
       XOR  DL, DL
       INT  10h
       POP  DX

       RET
ENDP

HighlightOn MACRO
       MOV  AL, 70h
       CALL Highlight
ENDM

HighlightOff MACRO
       MOV  AL, 07h
       CALL Highlight
ENDM

Start: STARTUPCODE

       CALL ReadFile
       CALL DisplayFile

       XOR  DX, DX                      ; zacneme na prvnim radku (DX=radek)
       HighlightOn                      ; (zvyraznime)

Main:  MOV  AH, 8                       ; vstup znaku
       INT  21h
       CMP  AL, 0                       ; sipky?
       JE   ExtKey
       CMP  AL, 1Bh                     ; ESC?
       JNE  Main
       EXITCODE 0                       ; ano=>konec

ExtKey:
       INT  21h                         ; sipky => scan-code=?
       CMP  AL, 50h                     ; sipka dolu?
       JE   GoDown
       CMP  AL, 48h                     ; sipka nahoru?
       JE   GoUp
       JMP  Main                        ; ne=>pokracujeme

GoDown:CMP  DL, MaxRow                  ; smime jeste dolu?
       JAE  Main                        ; ne => zpet do Main
       HighlightOff
       INC  DX
       HighlightOn
       JMP  Main

GoUp:  CMP  DX, 0                       ; smime jeste nahoru?
       JE   Main                        ; ne => zpet do Main

       HighlightOff
       DEC  DX
       HighlightOn
       JMP  Main

END