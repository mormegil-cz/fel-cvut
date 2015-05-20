; TGA.ASM - modul pro praci s TGA obrazky
; (c) 2000 by DragonSoft

; rozhrani a dokumentace rozhrani v 'TGA.INC'

INCLUDE common.inc

.DATA?

TGA_Header STRUC                        ; struktura hlavicky souboru TGA
           IDLength        DB ?
           CMAPType        DB ?
           ImageType       DB ?
           FirstCMAPEntry  DW ?
           CMAPLength      DW ?
           CMAPEntrySize   DB ?
           ImageXOrigin    DW ?
           ImageYOrigin    DW ?
           ImageWidth      DW ?
           ImageHeight     DW ?
           BitPerPixel     DB ?
           ImageDescriptor DB ?
ENDS

Header  TGA_Header ?
Palette DB 3*256 DUP (?)

.CODE

INCLUDE Display.inc             ; kvuli konstantam barev

PUBLIC _LoadTGA
PUBLIC _SetPalette

_LoadTGA PROC NEAR
ARG FileName:NEAR PTR BYTE, PicData:NEAR PTR BYTE = ArgLen

    ENTER 0, 0

    MOV  AX, 3D00h              ; Otevrit soubor, read-only
    MOV  DX, FileName
    INT  21h
    JC   @@Exit
    MOV  BX, AX                 ; BX = Handle

    ; Nacti hlavicku souboru
    MOV  DX, OFFSET Header
    MOV  CX, SIZE Header
    MOV  AH, 3Fh
    INT  21h
    JC   @@Close
    SUB  AX, CX                 ; prectena cela hlavicka?
    JNE  @@Error                ; ne=>chyba

    ; kontrola spravnosti formatu
    CMP  Header.ImageType, 1
    JNE  @@Error
    CMP  Header.ImageWidth, 320
    JA   @@Error
    CMP  Header.ImageHeight, 200
    JA   @@Error
    CMP  Header.BitPerPixel, 8
    JNE  @@Error
    CMP  Header.CMAPEntrySize, 24
    JNE  @@Error
    CMP  Header.FirstCMAPEntry, 0
    JNE  @@Error

    ; pripadne preskoceni ID retezce
    MOV  DL, Header.IDLength
    XOR  DH, DH
    TEST DX, DX
    JE   @@IDNotPresent

    XOR  CX, CX
    MOV  AX, 4201h              ; seek, rezim seFromCurrent
    INT  21h
    JC   @@Error
@@IDNotPresent:

    ; pripadne precteni palety
    MOV  CX, Header.CMAPLength
    TEST CX, CX
    JZ   @@CMAPNotPresent
    MOV  DX, CX
    SHL  DX, 1
    ADD  CX, DX
    CMP  CX, SIZE Palette
    JNE  @@Error

    MOV  DX, OFFSET Palette
    MOV  AH, 3Fh                ; cteni
    INT  21h
    JC   @@Error
    SUB  AX, CX                 ; prectena cela paleta?
    JNE  @@Error                ; ne=>chyba

    ; konverze palety RGB,8-bit na BGR,6-bit

    CLD
    PUSH DS
    POP  ES
    MOV  SI, DX
    MOV  DI, SI
    MOV  CX, 100h
@@NextPaletteByte:
    LODSB
    SHR  AL, 2
    XCHG AL, AH
    LODSB
    SHR  AL, 2
    XCHG AL, DL
    LODSB
    SHR  AL, 2
    STOSB
    XCHG AL, DL
    STOSB
    XCHG AL, AH
    STOSB
    LOOP @@NextPaletteByte
@@CMAPNotPresent:

    ; precteni dat obrazku
    MOV  AX, Header.ImageWidth
    MUL  Header.ImageHeight
    TEST DX, DX
    JNZ  @@Error                 ; prilis velky obrazek => chyba
    MOV  DL, Header.BitPerPixel
    SHR  DL, 3
    MUL  DX
    TEST DX, DX
    JNZ  @@Error                ; prilis velky obrazek => chyba

    MOV  CX, AX
    MOV  DX, PicData
    MOV  AH, 3Fh                ; cteni
    INT  21h
    JC   @@Error
    SUB  AX, CX                 ; precten cely obrazek?
    JZ   @@Close                ; ano=>ok, konec
                                ; ne=>chyba
@@Error:
    MOV   AX, -1                ; Nespecifikovana chyba
@@Close:
    ; zavri soubor
    PUSH  AX                     ; Uchovej priznak chyby
    MOV   AH, 3Eh
    INT   21h
    POP   AX                     ; a vrat ho
@@Exit:
    LEAVE
    RET  ArgLen
ENDP

_SetPalette PROC NEAR
     MOV  DX, 3C8h                      ; 3C8 = VGA palette out index
     XOR  AL, AL
     OUT  DX, AL
     INC  DX                            ; 3C9 = VGA palette data
     MOV  CX, SIZE Palette
     MOV  SI, OFFSET Palette

     ; preddefinovane barvy
     MOV  WORD PTR [SI+3*White], 03F3Fh
     MOV  BYTE PTR [SI+3*White+2], 03Fh
     MOV  WORD PTR [SI+3*Black], 0000h
     MOV  BYTE PTR [SI+3*Black+2], 00h
     MOV  WORD PTR [SI+3*Red], 003Fh
     MOV  BYTE PTR [SI+3*Red+2], 000h

     CLD
     REP  OUTSB

     RET
ENDP

END