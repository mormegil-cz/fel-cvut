%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Hlavni volani, pomocne vypisy, apod.                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Hlavni volani pro test
% - s definovanou deskou
test :-
  startpozice(SP),
  test_deska(SP).

% - s nahodnou deskou
test(Seed) :-
  nahodna(SP, Seed),
  test_deska(SP).

% - s konkretni deskou
test_deska(SP) :-
  kresli_desku(SP),
  vyres(SP, Postup),
  write('--> '), vypis(Postup), write(' -->'), nl,
  cilpozice(CP),
  kresli_desku(CP).

%% Hlavni volani:
vyres(Pozice, Postup) :-
  hodnota_stavu(Pozice, HS),
  hledej_reseni([(Pozice, 0+HS, [])], Postup).

%% Vypis postupu
vypis([]).
vypis([P|R]) :-
  vypis(R),
  write(P),
  write(' ').

%% Nakresleni jednoho kamene nebo volneho mista
wr(space) :- write(' '), !.
wr(X)     :- write(X).

%% Nakresleni desky
kresli_desku(deska(A,B,C,D,E,F,G,H,I)) :-
  wr(A), wr(B), wr(C), nl,
  wr(D), wr(E), wr(F), nl,
  wr(G), wr(H), wr(I), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Prohledavani stavoveho prostoru (algoritmus A*)         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Stav = (deska(...), F+G, TahyDosud)
%% TahyDosud ... napr. [nahoru, vlevo, nahoru, vpravo]
%% F ... celkova cena TahyDosud
%% G ... hodnota_stavu(deska(...))

%% Obecne prohledavani
%% hledej_reseni(ZnameStavy, PostupKCili)
hledej_reseni([(Deska, _, Postup) | _], Postup) :-
    cilpozice(Deska),
    !.
hledej_reseni([Stav | Zbytek], Postup) :-
    prodluz(Stav, NoveStavy),
    nalej(NoveStavy, Zbytek, Stavy1),
    unikatni(Stavy1, UStavy1),                 % Uchovavej vzdy jen nejkratsi kopii stavu
    orizni(UStavy1, 16, RUStavy1),             % Uchovavej jenom 16 nejperspektivnejsich stavu
    hledej_reseni(RUStavy1, Postup).

%% orizni(Seznam, MaxDelka, OrezanySeznam)
%% Pokud ma Seznam vetsi delku nez MaxDelka, vrati pouze prvnich MaxDelka polozek v OrezanySeznam, jinak cely.
orizni(_, 0, []) :- !.
orizni([], _, []).
orizni([H|T], MaxDelka, [H|OT]) :-
  MD1 is MaxDelka-1,
  orizni(T, MD1, OT). 

%% nalej(NoveStavy, StareStavy, Vysledek).
%% sleje NoveStavy se StareStavy do Vysledek (vse setridene podle F+G)
nalej([], X, X).
nalej(X, [], X).
nalej([(D1, F1+G1, P1)|Tail1], [(D2, F2+G2, P2)|Tail2], [(D1, F1+G1, P1)|TailV]) :-
    H1 is F1+G1,
    H2 is F2+G2,
    H1 =< H2, !,
    nalej(Tail1, [(D2, F2+G2, P2)|Tail2], TailV).
nalej([(D1, F1+G1, P1)|Tail1], [(D2, F2+G2, P2)|Tail2], [(D2, F2+G2, P2)|TailV]) :-
    %% H1 is F1+G1,
    %% H2 is F2+G2,
    %% H1 >= H2,  -- viz ! vyse
    nalej([(D1, F1+G1, P1)|Tail1], Tail2, TailV).

%% unikatni(Stavy, UnikatniStavy)
%% Zajisti, aby kazdy unikatni stav ze Stavy byl v UnikatniStavy prave jednou (s nejmensim ohodnocenim)
unikatni([], []).                     %% trivialni pripad: prazdny seznam
unikatni([H|R1], [H|R2]) :-           %% prvek patri do seznamu unikatu, pokud
  not obsahuje_stav(H, R1),           %% se jiz ve zbytku nevyskytuje
  unikatni(R1, R2),                   %% a zbytek je seznam unikatu
  !.
unikatni([_|Rest], Rest).             %% Jinak do seznamu unikatu nepatri.

%% obsahuje_stav(Stav, SeznamStavu)
%% SeznamStavu obsahuje stav se stejnou deskou jako ma Stav.
obsahuje_stav((X, _+_, _), [(X, _+_, _) | _]) :- !.
obsahuje_stav((X, A+B, C), [_|T]) :-
  obsahuje_stav((X, A+B, C), T).

%% prodluz(Stav, NoveStavy)
%% Ziskej vsechny stavy, do kterych se lze dostat jednim tahem ze stavu Stav
prodluz((Deska, F+_, Postup), NoveStavy) :-
    Fn is F+1,
    bagof((PoTahu, Fn+Gn, NovyPostup),
          dalsitah(Deska, PoTahu, Gn, Postup, NovyPostup),
          BagStavy),
    setrid_stavy(BagStavy, NoveStavy),
    !.
prodluz(_, []).                   %% Pokud bagof nedokaze najit jediny, selze, pak uspeje tenhle.

%% setrid_stavy(SeznamStavu, SetridenySeznamStavu)
%% Setridi SeznamStavu podle F+G a ulozi do SetridenySeznamStavu. (Insert-sort)
setrid_stavy([], []).
setrid_stavy([Stav|Seznam], Setrideno) :-
    setrid_stavy(Seznam, Seznam1),
    vloz(Stav, Seznam1, Setrideno).

%% vloz(Stav, SeznamStavu, SeznamStavuPoVlozeni)
%% Vlozi Stav do setrideneho SeznamStavu, tak aby SeznamStavuPoVlozeni byl setrideny
vloz(Stav, [], [Stav]).
vloz((D1, F1+G1, P1), [(D2, F2+G2, P2)|Tail2], [(D1, F1+G1, P1), (D2, F2+G2, P2) | Tail2]) :-
    H1 is F1+G1,
    H2 is F2+G2,
    H1 =< H2,
    !.
vloz((D1, F1+G1, P1), [(D2, F2+G2, P2)|Tail2], [(D2, F2+G2, P2) | TailV]) :-
    % H1 is F1+G1,
    % H2 is F2+G2,
    % H1 >= H2,    -- viz ! vyse
    vloz((D1, F1+G1, P1), Tail2, TailV).

%% dalsitah(Deska, DeskaPoTahu, Postup, PostupPoTahu)
%% ze stavu Deska (dosazenem pomoci postupu Postup) se lze dostat jednim tahem do DeskaPoTahu
%% (tak, ze cely postup pak vypada jako PostupPoTahu)
dalsitah(Deska, DeskaPoTahu, G, Postup, PostupPoTahu) :-
    tah(Deska, DeskaPoTahu, Tah),
    (                   % dalsi tah nesmi jit proti poslednimu (pokud nejaky byl)
     Postup = []
     ;
     ([PosledniTah | _] = Postup, not protitah(PosledniTah, Tah))
    ),
    hodnota_stavu(DeskaPoTahu, G),
    PostupPoTahu = [Tah|Postup].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Heuristika                                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Protitahy:
protitah(nahoru, dolu).
protitah(dolu, nahoru).
protitah(vlevo, vpravo).
protitah(vpravo, vlevo).

%% hodnota_stavu(deska(...), HodnotaStavu)
%% Heuristicke ohodnoceni stavu desky:
hodnota_stavu(deska(A,B,C,D,E,F,G,H,I), Hodnota) :-
    !,
    hodnota_kamene(deska(A,B,C,D,E,F,G,H,I), a, 1, 1, Ah),
    hodnota_kamene(deska(A,B,C,D,E,F,G,H,I), b, 2, 1, Bh),
    hodnota_kamene(deska(A,B,C,D,E,F,G,H,I), c, 3, 1, Ch),
    hodnota_kamene(deska(A,B,C,D,E,F,G,H,I), h, 1, 2, Hh),
    hodnota_kamene(deska(A,B,C,D,E,F,G,H,I), space, 2, 2, Sh),
    hodnota_kamene(deska(A,B,C,D,E,F,G,H,I), d, 3, 2, Dh),
    hodnota_kamene(deska(A,B,C,D,E,F,G,H,I), g, 1, 3, Gh),
    hodnota_kamene(deska(A,B,C,D,E,F,G,H,I), f, 2, 3, Fh),
    hodnota_kamene(deska(A,B,C,D,E,F,G,H,I), e, 3, 3, Eh),
    Hodnota is (Ah + Bh + Ch + Dh + Eh + Fh + Gh + Hh + Sh).

%% hodnota_kamene(deska(...), HledanyKamen, SpravnaX, SpravnaY, HodnotaKamene)
%% Heuristicke ohodnoceni jednoho kamene (= vzdalenost od spravne pozice v metrice Manhattan)
hodnota_kamene(deska(A,B,C,D,E,F,G,H,I), Kamen, X0, Y0, Hodnota) :-
    !,
    najdi_kamen(A,B,C,D,E,F,G,H,I,Kamen,Xk,Yk),
    Dx is abs(X0-Xk),
    Dy is abs(Y0-Yk),
    Hodnota is Dx+Dy.

%% najdi_kamen(K1,K2,...,K9,HledanyKamen,PoziceX,PoziceY)
%% Zjisti souradnice [PoziceX, PoziceY] HledanyKamen na desce deska(K1,K2,...,K9)
najdi_kamen(K,_,_,_,_,_,_,_,_,K,1,1) :- !.
najdi_kamen(_,K,_,_,_,_,_,_,_,K,2,1) :- !.
najdi_kamen(_,_,K,_,_,_,_,_,_,K,3,1) :- !.
najdi_kamen(_,_,_,K,_,_,_,_,_,K,1,2) :- !.
najdi_kamen(_,_,_,_,K,_,_,_,_,K,2,2) :- !.
najdi_kamen(_,_,_,_,_,K,_,_,_,K,3,2) :- !.
najdi_kamen(_,_,_,_,_,_,K,_,_,K,1,3) :- !.
najdi_kamen(_,_,_,_,_,_,_,K,_,K,2,3) :- !.
najdi_kamen(_,_,_,_,_,_,_,_,K,K,3,3) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tvorba nahodne desky pro testovani                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nahodna(Deska, Seed) :-
  cilpozice(CD),
  michej(CD, Deska, 500, Seed).

michej(D, D, 0, _) :- !.
michej(D1, D2, Cnt, Seed) :-
  shuffle(D1, Dn, Seed, Seed2),
  Cnt1 is Cnt-1,
  michej(Dn, D2, Cnt1, Seed2).

shuffle(D1,D2,Seed1,Seed2) :-
  random4(Seed1, R, Seed2),
  !,
  nahodny_tah(R, Tah),
  (tah(D1,D2,Tah); (protitah(Tah, Protitah), tah(D1,D2,Protitah))).

nahodny_tah(0, nahoru).
nahodny_tah(1, vlevo).
nahodny_tah(2, vpravo).
nahodny_tah(3, dolu).

%% random4(CurrentSeed, RandomNumber, NewSeed)
%% Generuje nahodne cislo RandomNumber z {0,1,2,3}, s pouzitim CurrentSeed. Hodnota pro pristi volani je NewSeed.
random4(Seed1, RandomNumber, Seed2) :-
   Seed2 is Seed1 * 16'08088405 + 1,
   RandomNumber is abs(Seed2 >> 30).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Pravidla hry                                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Uchovavani stavu desky:
%% deska(A,B,C,D,E,F,G,H,I)
%% +---+
%% |ABC|
%% |DEF|
%% |GHI|
%% +---+
%% A-I ... a..h,space
%% space oznacuje prazdne misto

%% Pocatecni pozice:
startpozice(deska(g,e,h,b,space,d,a,f,c)).

%% Cilova pozice:
cilpozice(deska(a,b,c,h,space,d,g,f,e)).

%% Mozne tahy:
%% tah(DeskaPred, DeskaPo, NazevTahu)
%% NazevTahu ... nahoru,dolu,vlevo,vpravo

tah(deska(space,B,C,D,E,F,G,H,I),deska(D,B,C,space,E,F,G,H,I),nahoru).
tah(deska(space,B,C,D,E,F,G,H,I),deska(B,space,C,D,E,F,G,H,I),vlevo).
tah(deska(A,space,C,D,E,F,G,H,I),deska(space,A,C,D,E,F,G,H,I),vpravo).
tah(deska(A,space,C,D,E,F,G,H,I),deska(A,E,C,D,space,F,G,H,I),nahoru).
tah(deska(A,space,C,D,E,F,G,H,I),deska(A,C,space,D,E,F,G,H,I),vlevo).
tah(deska(A,B,space,D,E,F,G,H,I),deska(A,space,B,D,E,F,G,H,I),vpravo).
tah(deska(A,B,space,D,E,F,G,H,I),deska(A,B,F,D,E,space,G,H,I),nahoru).
tah(deska(A,B,C,space,E,F,G,H,I),deska(space,B,C,A,E,F,G,H,I),dolu).
tah(deska(A,B,C,space,E,F,G,H,I),deska(A,B,C,E,space,F,G,H,I),vlevo).
tah(deska(A,B,C,space,E,F,G,H,I),deska(A,B,C,G,E,F,space,H,I),nahoru).
tah(deska(A,B,C,D,space,F,G,H,I),deska(A,space,C,D,B,F,G,H,I),dolu).
tah(deska(A,B,C,D,space,F,G,H,I),deska(A,B,C,space,D,F,G,H,I),vpravo).
tah(deska(A,B,C,D,space,F,G,H,I),deska(A,B,C,D,F,space,G,H,I),vlevo).
tah(deska(A,B,C,D,space,F,G,H,I),deska(A,B,C,D,H,F,G,space,I),nahoru).
tah(deska(A,B,C,D,E,space,G,H,I),deska(A,B,space,D,E,C,G,H,I),dolu).
tah(deska(A,B,C,D,E,space,G,H,I),deska(A,B,C,D,space,E,G,H,I),vpravo).
tah(deska(A,B,C,D,E,space,G,H,I),deska(A,B,C,D,E,I,G,H,space),nahoru).
tah(deska(A,B,C,D,E,F,space,H,I),deska(A,B,C,space,E,F,D,H,I),dolu).
tah(deska(A,B,C,D,E,F,space,H,I),deska(A,B,C,D,E,F,H,space,I),vlevo).
tah(deska(A,B,C,D,E,F,G,space,I),deska(A,B,C,D,space,F,G,E,I),dolu).
tah(deska(A,B,C,D,E,F,G,space,I),deska(A,B,C,D,E,F,space,G,I),vpravo).
tah(deska(A,B,C,D,E,F,G,space,I),deska(A,B,C,D,E,F,G,I,space),vlevo).
tah(deska(A,B,C,D,E,F,G,H,space),deska(A,B,C,D,E,space,G,H,F),dolu).
tah(deska(A,B,C,D,E,F,G,H,space),deska(A,B,C,D,E,F,G,space,H),vpravo).

