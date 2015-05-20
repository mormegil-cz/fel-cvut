; 3. Domaci ukol ze SOJ (1.cast)
; Petr Kadlec, cviceni ze SOJ ve ctvrtek od 9:15

; Zvolene zadani:
;         prerovnejte pole bytu tak, aby nejdrive byly vsechna suda cisla,
;         pote vsechna licha cisla. - Pomoci procedury

; Pouzity algoritmus:
;         Pole se prochazi soucasne odpredu i odzadu. Zepredu se najde
;         prvni liche cislo, odzadu prvni sude cislo. Tato se prohodi
;         a pokracuje se tak dlouho, az se ukazatele stretnou

CODE SEGMENT
ASSUME CS:CODE, DS:CODE, ES:CODE

.186

ORG 100h

Start:  CLD                        ; Budeme pouzivat retezove instrukce

        MOV  AX, OFFSET PoleStart   ; [SI] obsahuje levy zpracovavany prvek
        PUSH AX
        MOV  AX, (OFFSET PoleKonec)-(OFFSET PoleStart)
        PUSH AX

        CALL SortOddEven

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


; volani: konvence Pascal
;         procedure SortOddEven(OfsPole:Word; Delka:Word);
SortOddEven PROC NEAR

       ENTER 0,0                   ; vstupni uprava zasobniku (stack frame)
       IRP  reg,<SI,DI,AX>         ; ulozeni obsahu pouzitych registru
            PUSH reg
       ENDM

       MOV  SI, [BP+6]             ; prvni parametr - [SI] obsahuje levy zpracovavany prvek
       MOV  DI, SI                 ; [DI] obsahuje pravy zpracovavany prvek
       ADD  DI, [BP+4]             ; druhy parametr je delka pole

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

       IRP  reg,<AX,DI,SI>         ; obnoveni obsahu pouzitych registru
            POP reg
       ENDM

       LEAVE                       ; zruseni stack frame
       RET  4                      ; konec

SortOddEven ENDP

; ***************** Data pole *****************
PoleStart LABEL NEAR
  DB 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
PoleKonec LABEL NEAR

; -- konec programu --
CODE ENDS
END START