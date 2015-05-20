; 4. domaci ukol ze SOJ

.MODEL SMALL
.186
.STACK 2048
.DATA

Line_Len   EQU 60                                       ; delka radky (znaku)

WordBuffer DB Line_Len DUP (?)                          ; buffer na slovo

InFileName DB "ALIGN.IN",0                              ; jmeno vst. souboru
ErrorMsg   DB "I/O chyba pristupu k ALIGN.IN",13,10,"$" ; chybova zprava
Slash      DB "\"                                       ; znak pro pokracovani na dalsim radku (nasledovan CRLF!)
CRLF       DB 13,10,"$"                                 ; konec radky

.CODE

DumpBuffer PROC NEAR            ; vypis WordBuffer (az k DX)
         PUSH CX
         MOV  CX, DX
         MOV  SI, OFFSET WordBuffer
         SUB  CX, SI
         JZ   @@Exit            ; prazdny ==> exit

         MOV  AH, 2             ; vypis znak
@@NextChar:
         MOV  DL, [SI]
         INT  21h
         INC  SI
         LOOP @@NextChar

@@Exit:
         POP  CX
         MOV  DX, OFFSET WordBuffer     ; WordBuffer := prazdny
         RET
ENDP

Start:  STARTUPCODE

        ; otevri soubor
        MOV  AX, 03D00h                 ; otevri soubor, read-only
        MOV  DX, OFFSET InFileName
        INT  21h
        JC   Error
        MOV  BX, AX                     ; BX = handle

        MOV  DX, OFFSET WordBuffer      ; DX = soucasna pozice ve wordbufferu
        XOR  CX, CX                     ; CX = pozice na radce

MainLoop:
        PUSH CX
        MOV  AH, 3Fh                    ; precti jeden znak
        MOV  CX, 1
        INT  21h
        POP  CX
        JC   ReadError                  ; chyba cteni
        TEST AX, AX                     ; AX = 0 ==> EOF
        JZ   EOF
        MOV  SI, DX                     ; jaky znak jsme precetli?
        LODSB
        INC  DX
        INC  CX
        CMP  AL, " "                    ; mezera => vypis buffer
        JE   IsSpace
        CMP  AL, 10                     ; CR => vypis buffer (&odradkuj)
        JE   IsCRLF
        CMP  AL, 13                     ; LF => vypis buffer (&odradkuj)
        JE   IsCRLF
                                        ; jinak - je cela radka bez mezery?
        CMP  DX, (OFFSET WordBuffer)+Line_Len-1
        JE   TooLong
        CMP  CX, Line_Len               ; ne => uz jsme na konci radku?
        JB   MainLoop                   ;       ne => pokracujeme
        PUSH DX                         ;       ano => odradkujeme
        MOV  DX, OFFSET CRLF
        MOV  AH, 9
        INT  21h
        POP  DX
        MOV  CX, DX                     ;       posuneme se na zacatek radku
        SUB  CX, OFFSET WordBuffer
        JMP  MainLoop                   ;       a pokracujeme
TooLong:CALL DumpBuffer                 ; dlouha radka bez mezery - vypiseme
        MOV  DX, OFFSET Slash           ; "\" a pokracujeme
        MOV  AH, 9
        INT  21h
        XOR  DX, DX
        XOR  CX, CX                     ; na novem radku
        JMP  MainLoop                   ; a pokracujeme
IsCRLF: CALL DumpBuffer                 ; konec radky => vypis buffer & odradkuj
        XOR  CX, CX
        JMP  MainLoop                   ; a pokracujeme
IsSpace:CALL DumpBuffer                 ; mezera => vypis buffer
        JMP  MainLoop

EOF:    CALL DumpBuffer                 ; EOF => vypis pripadny obsah bufferu
        ; zavri soubor
        MOV  AH, 3Eh
        INT  21h

        EXITCODE 0                      ; that's all, folks

ReadError: ; zavri soubor a nahlas chybu
        MOV  AH, 3Eh
        INT  21h

Error:  ; nahlas chybu
        MOV  DX, OFFSET ErrorMsg
        MOV  AH, 9
        INT  21h

        EXITCODE 1                      ; abnormal program termination

END