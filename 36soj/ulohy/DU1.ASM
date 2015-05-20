; 1. Domaci ukol ze SOJ
; Petr Kadlec, cviceni ze SOJ ve ctvrtek od 9:15

; Zvolene zadani:
;         prerovnejte pole bytu tak, aby nejdrive byly vsechna suda cisla,
;         pote vsechna licha cisla.

; Pouzity algoritmus:
;         Pole se prochazi soucasne odpredu i odzadu. Zepredu se najde
;         prvni liche cislo, odzadu prvni sude cislo. Tato se prohodi
;         a pokracuje se tak dlouho, az se ukazatele stretnou

CODE SEGMENT
ASSUME CS:CODE, DS:CODE, ES:CODE

ORG 100h

Start:  CLD                        ; Budeme pouzivat retezove instrukce

        MOV  SI, OFFSET PoleStart   ; [SI] obsahuje levy zpracovavany prvek
        MOV  DI, OFFSET PoleKonec-1 ; [DI] obsahuje pravy zpracovavany prvek

HledejVlevo: ; Hledej prvni liche cislo zleva
        CMP  SI, DI                ; Uz jsme se stretli?
        JAE  Konec                 ; Ano -> konec
        LODSB                      ; Vezmi dalsi prvek pole
        TEST AL, 1                 ; Je to liche cislo?
        JZ   HledejVlevo           ; Ne -> hledej dal

HledejVpravo: ; Hledej prvni sude cislo zprava
        CMP  SI, DI                ; Uz jsme se stretli?
        JA   Konec                 ; Ano -> konec
        MOV  AL, [DI]              ; Vezmi dalsi prvek pole
        DEC  DI                    ; Posun se o jeden prvek vlevo
        TEST AL, 1                 ; Je to sude cislo?
        JNZ  HledejVpravo          ; Ne -> hledej dal

        ; Byly nalezeny dva prvky, ktere nyni musime vymenit
        XCHG AL, [SI-1]            ; V AL je sude cislo zprava, vymenime ho
                                   ; s lichym cislem zleva
        MOV  [DI+1], AL            ; Toto liche cislo umistime doprava
        ; pozn: +/-1 jsou zde protoze jsme se mezitim uz posunuli na dalsi byte
        JMP  SHORT HledejVlevo     ; A pokracujeme od zacatku

Konec:  ; Algoritmus skoncen. Nejdrive jsou vsechna suda cisla, dal jsou licha.


        ; KONEC DOMACIHO UKOLU PODLE ZADANI

        ; Pro moznost otestovani nyni vypiseme, zda jsou vsechna cisla Suda,
        ; nebo Licha
        MOV  SI, OFFSET PoleStart
        MOV  CX, (OFFSET PoleKonec)-(OFFSET PoleStart)
        TEST CX, CX
        JZ   Exit
        MOV  AH, 2
NextByte:
        LODSB
        MOV  DL, "S"
        TEST AL, 1
        JZ   Sude
        MOV  DL, "L"
Sude:   INT  21h
        LOOP NextByte

Exit:   RET

; ***************** Data pole *****************
PoleStart LABEL NEAR
  DB 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
PoleKonec LABEL NEAR

; -- konec programu --
CODE ENDS
END START