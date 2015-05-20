
                     Popis emulatoru pripravku SEMAFOR

verze 1.1 (release date 12.4.2000)

1. Uvod
~~~~~~~
 Program EMULATOR je urcen k emulaci hardwaroveho pripravku SEMAFOR,
coz je model krizovatky, pouzivane pri vyuce predmetu UPS na FEL CVUT.
Jelikoz pripravek SEMAFOR je k dispozici jen v pocitacovych laboratorich,
student muze sve programy testovat pouze v techto laboratorich. Tento
nedostatek resi prave tento emulator. Program EMULATOR po svem spusteni
monitoruje jemu predany testovany program a "odchytava" vsechny prime
pristupy na sledovany port paralelniho rozhrani LPT1 (porty 378h a 379h).
Tyto pristupy nahrazuje emulaci chovani pripravku, pricemz soucasne
zobrazuje prislusny stav pripravku na obrazovku, kde je zobrazen nakres
krizovatky, podobny pripravku SEMAFOR.

2. Navod k pouziti
~~~~~~~~~~~~~~~~~~
 Program se spusti prikazem "EMULATOR program" (bez uvozovek), kde program
je jmeno programu, za jehoz behu se ma emulovat pripravek. Tj. napr.
"EMULATOR semaf1". Po zadani tohoto prikazu se zobrazi obrazek krizovatky
a emulovany program je spusten. Za behu emulatoru funguji nasledujici
klavesy:
        ESC          Ukonceni emulovaneho programu
        Sipka nahoru Emulace stisku tlacitka v severni casti krizovatky
        Sipka vlevo  Emulace stisku tlacitka v zapadni casti krizovatky
        Sipka dolu   Emulace stisku tlacitka v jizni casti krizovatky
        Sipka vpravo Emulace stisku tlacitka ve vychodni casti krizovatky
        F1-F4        Emulace prepnuti vypinace 1-4
        F5           Obnoveni graficke obrazovky. Po stisknuti teto klavesy
                     je znovu nastaven rezim 320x200x256, nastavena spravna
                     paleta a zobrazen inicialni stav pripravku, tzn. vsechny
                     semafory a LED zhasnuty, vypinace v poloze 0. Tzn. po
                     stisku teto klavesy neni zobrazen skutecny stav
                     pripravku az do doby, nez emulovany program znovu
                     vysle na port prislusna data! Nemelo by byt zapotrebi
                     pouzivat tuto klavesu - je nutna pouze v pripade, ze
                     emulovany program nedodrzuje podminky z odstavce 3.
        Ctrl+Alt+Shift+ESC
                     Tvrde ukonceni emulovaneho programu - velmi drsne,
                     doporucuje se pouzivat pouze (!) ve stavu nouze,
                     s vysokou pravdepodobnosti ma za nasledek krach pocitace.
 Po ukonceni emulovaneho programu se emulator ukonci. Pokud byly v prubehu
emulace zjisteny nejake chyby (napr. cteni z portu urceneho pro vystup),
je jeste zobrazena informace o detekovanych chybach.

3. Technicke informace
~~~~~~~~~~~~~~~~~~~~~~
 Program EMULATOR funguje na principu krokovani programu, ktere je na
procesorech rady x86 umozneno nastavenim priznaku TF v registru (E)Flags.
Timto zpusobem je sledovana kazda instrukce provadena programem. Pokud je
zachycena instrukce "OUT DX,AL", nebo "IN AL, DX", ktera pristupuje na jeden
ze sledovanych portu, neni provedena, ale je emulovan jeji vysledek s
pouzitim HW pripravku. Emulator na sebe presmerovava vektory preruseni
INT1 (krokovani), INT3 (breakpoint) a INT9 (klavesnice).
 Program vyzaduje procesor minimalne 80286 a MSDOS nejmene verze 5.0.
 Z principu funkce programu je zrejme, ze je nezbytne nutne, aby program:
       * nevynuloval priznak TF v registru (E)Flags
          Pozn: Program se tomuto snazi do jiste miry zabranit, takze beznymi
          metodami by program nemel byt schopen tento priznak zmenit.
       * na sledovane porty pristupoval primo, tj. pomoci instrukci
         OUT DX, AL a IN AL, DX. Pristup napr. pomoci sluzeb BIOSu (pres
         INT 17h) neni sledovan a bude ignorovan.
       * nepresmeroval preruseni INT1, INT3, popr. INT9.
          Pozn: Preruseni INT1 a INT3 jsou pouzivano vetsinou debuggeru,
          takze prakticky neni mozno program bezici pod emulatorem jeste
          odladovat dalsim debuggerem (napr. pomoci TD)
       * nezmenil videorezim - emulator pred spustenim programu nastavi
         videorezim na 320x200x256 (rezim 13h) a nastavi vlastni paletu
         barev. Pokud vas program zmeni videorezim, nebude na obrazovce
         zobrazovan stav pripravku. Pokud se toto stane, je mozno stisknout
         klavesu F5 pro refresh - viz 2. odstavec.
 V soucasne verzi je nekolik omezeni, na ktera je treba upozornit. Je mozne,
ze v dalsi verzi budou tato omezeni odstranena:
       * Na sledovane porty nelze pristupovat pomoci 16-bitoveho pristupu,
         tj. pomoci instrukci OUT DX, AX, resp. IN AX, DX.
       * Jsou podporovany pouze programy typu EXE, programy typu COM jsou
         emulatorem odmitnuty.
       * Emulator je schopen pracovat pouze s EXE soubory, jejichz velikost
         nepresahuje cca 64KB.
POZOR! Je asi potreba upozornit, ze pouzity mechanismus ma za nasledek znacne
zpomaleni behu emulovaneho programu! Zpomaleni je zavisle na strukture
programu, ale prumerne by mohlo jit odhadnout na cca 1000-nasobne zpomaleni!!!
 Tzn. pokud Vas program pouziva (dost spatny) kod pro zpomaleni zalozeny na
ukazkovem prikladu, tzn. preddefinovany pocet instrukci, pak tato cekaci
smycka bude cekat pod emulatorem _mnohem_ (!) dele nez bez emulatoru.
 Proto bud pouzijte nejaky zpusob mereni realneho casu (napr. pomoci BIOSu),
nebo znacne snizte doby cekani.

4. Potrebne soubory
~~~~~~~~~~~~~~~~~~~
 K programu EMULATOR patri nasledujici soubory:
   EMULATOR.EXE (9088 byte)  Samotny program
   CROSS.RAW    (64000 byte) Obrazek krizovatky pouzivany programem
   EMULATOR.TXT (6241 byte)  Tento dokument

5. Chyby v programu
~~~~~~~~~~~~~~~~~~~
 Je mozne (a take dost pravdepodobne), ze program obsahuje nejake chyby. Pokud
nejakou chybu odhalite, dejte mi prosim vedet.

6. Sireni programu
~~~~~~~~~~~~~~~~~~
 Tento program je FREEWARE. Muzete jej libovolne kopirovat, za podminky,
ze jej budete sirit v nezmenene podobe a vcelku, vcetne tohoto dokumentu.

7. Autor
~~~~~~~~
 Autora programu EMULATOR muzete kontaktovat na e-mailove adrese
mormegil@centrum.cz

---------------------------------------------------------------------------
konec souboru