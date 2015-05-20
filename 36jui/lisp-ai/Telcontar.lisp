(in-package "BLUDISTE")
(provide "Telcontar")
(require "postava" "postava")
(require "tahovy_objekt" "tahovy_objekt")
(require "parametry_tahu" "parametry_tahu")
(require "IRS" "IRS")
(require "utils" "utils")
(require "Sidewinder" "Sidewinder")
(require "AMRAAM" "AMRAAM")

(proclaim '(optimize (compilation-speed 0) (debug 0) (safety 0) (space 0) (speed 3)))

;;; Tento soubor obsahuje funkce pro postavicku Telcontar.

;;; Chyby, mozna budouci vylepseni:
;;;   - vyuzivat moznost vracet heuristikou (smer . hodnota), hlavne vypocitavat hodnotu nejak inteligentne, ne tak
;;;     pevne, jako je to ted
;;;   - v soucasnosti je tu nekolik oddelenych stupnu ohodnocovani, lepsi by bylo propojit je: pri hledani nejblizsiho
;;;     objektu daneho typu se zjisti smery k objektum se stejnou vzdalenosti a vybere se ten s nejvyssi koncentraci,
;;;     ovsem informace o koncentraci je ihned zahozena a v hodnoceni se ignoruje. To by slo zlepsit.
;;;   - navigace vubec nepocita s otacenim, tzn. bere "nahoru, doleva, nahoru, doleva, ..." stejne, jako
;;;     "nahoru, nahoru, ..., doleva, doleva, ...", coz vuuubec stejne neni...
;;;   - pri vstupu do vychodu se stale aplikuje pravidlo o chuzi vpred, coz je nyni zbytecne, je mozno usetrit jeden tah
;;;     tim, ze tam vstoupim stranou/zacouvam

(defclass Telcontar (trida_tahovy_objekt)
  (
   (mapa :initarg :mapa :accessor getMapa :initform (make-instance 'MapaIRS) :type MapaIRS)      ; IRS
  )
  (:documentation "Mozek postavicky Telcontar")
)

(defmethod POUZIJ-PRIJATOU-MAPU ((Self Telcontar) (PrijataMapa trida_mapa))
  (POUZIJ-MAPU (getMapa Self) PrijataMapa)
)

(defmethod PROVED-VYBRANY-TAH ((Self Telcontar) (Tah trida_tah))
  (PROVEDEN-TAH (getMapa Self) Tah)
)

(defun NAJDI-SMER-K (Smerovani X Y)
  ;; V poli Smerovani zjisti smer z pocatku na policko X Y
  (let ((AktSmerovani) (PosledniSmerovani 0) (SmerDelta))
    (loop
      (setf AktSmerovani (elt (elt Smerovani Y) X))
      (when (zerop (car AktSmerovani)) (return))
      (setf PosledniSmerovani (cdr AktSmerovani))
      (setf SmerDelta (SMER-DELTA PosledniSmerovani))
      (incf X (car SmerDelta))
      (incf Y (cdr SmerDelta))
    )
    (mod (+ PosledniSmerovani 2) 4)
) )

(defun NAJDI-MINIMUM-A-SMER (Vzdalenosti Smerovani)
  ;; Najde v poli Vzdalenosti nejblizsi a dohleda smer k nemu, vrati ( vzdal . smer ), pokud je vsechno nil, vrati nil
  ;;  pokud je vice stejne blizkych, vybere smer, kterym se dostane k nejvice z nich
  (let (
        (MinDist nil)
        ;(X 0)
        ;(Y 0)
        (Hlasu)
        (Radek)
        (Policko)
       )
    (dotimes (Y (length Vzdalenosti))
    ;(dolist (Radek Vzdalenosti)
      ;(setf X 0)
      (setf Radek (elt Vzdalenosti Y))
      (dotimes (X (length Radek))
      ;(dolist (Policko Radek)
        (setf Policko (elt Radek X))
        (when Policko
          (when (or (null MinDist) (< (car Policko) MinDist))
            (setf MinDist (car Policko))
            (setf Hlasu (make-array 4 :initial-element 0))
            ;(break)
          )
          (when (= MinDist (car Policko))
            (incf (elt Hlasu (NAJDI-SMER-K Smerovani X Y)))
            ;(break)
        ) )
        ;(incf X)
      )
      ;(incf Y)
    )
    ;(break)
    ;(format t "Nejlepsi smery, se vzdalenosti ~d: ~g~%" MinDist Hlasu)
    (when MinDist
      (cons MinDist (position (reduce #'max Hlasu) Hlasu))
) ) )

(defmethod NAJDI-NEJBLIZSI ((Self Telcontar) (CoHledat symbol))
  ;; Najde nejblizsi znamy objekt typu CoHledat, vrati (vzdalenost . smer) k nemu, nebo nil pokud o zadnem takovem nevi
  (let* (
         (IRS (getMapa Self))
         (Mapa (GET-MAPA IRS))
         (Smerovani (slot-value IRS 'smerovani))
         (Vzdalenosti (map2 'vector (lambda (Pole SmerPolozka) (when (eq Pole CoHledat) SmerPolozka)) Mapa Smerovani))
        )

     (NAJDI-MINIMUM-A-SMER Vzdalenosti Smerovani)
) )

(defmethod DELAM-KROK-K-NESTVURE ((Krok trida_tah) (Self Telcontar) (Parametry trida_parametry_tahu))
  ;; Pokud by me krok privedl na mene nez jedno policko k nestvure, vrati neco ne-nil, jinak nil.
  (when (eq (slot_akce Krok) 'dopredu)
    (let (
          (PozicePriser (mapcar #'slot_pozice (slot_viditelne_prisery Parametry)))
         )
      (remove-if (lambda (Pozice) (> (+ (abs (slot_x Pozice)) (abs (1- (slot_y Pozice)))) 1)) PozicePriser)
) ) )

(defmethod DELAM-KROK-DO-STRELY ((Krok trida_tah) (Self Telcontar) (Parametry trida_parametry_tahu))
  ;; Pokud by me krok privedl do strely, vrati neco ne-nil, jinak nil.
  (COMMENT "@todo: FIXME! Kontrolovat i jine kroky, nez dopredu, zda nejdou do strely!")
  (when (eq (slot_akce Krok) 'dopredu)
    (let (
          (PoziceStrel (mapcar #'slot_pozice (slot_viditelne_strely Parametry)))
         )
      (remove-if (lambda (Pozice) (plusp (+ (abs (slot_x Pozice)) (abs (1- (slot_y Pozice)))))) PoziceStrel)
) ) )

;; Nastavitelne konstanty pro mozek:
(defparameter *TBCMinTime* 100/100)      ; procento zbyvajiciho casu pred uprkem do vychodu [100%]
(defparameter *TBCNoExitTimeCoeff* 3/2)  ; koeficient potrebneho casu, pokud zadny vychod dosud neznam [150%]
(defparameter *TBCEnoughMissiles*  8)    ; "dostatecne" mnozstvi strel, dalsi nevyhledavam
(defparameter *TBCMinMissilesLongerShoot* 9) ; minimalni mnozstvi strel pro strelbu na "delsi" vzdalenost (Sidewinder)
(defparameter *TBCMinMissilesLongShoot* 4) ; minimalni mnozstvi strel pro strelbu na "dlouhou" vzdalenost (AMRAAM)
(defparameter *TBCMustHaveMissiles* 2)   ; kolik musi mit neustale strel, pokud nema, jde si pro ne

(defmethod HEUR-SPECHEJ ((Self Telcontar) (Parametry trida_parametry_tahu))
  ;; Heuristika: pokud je nejblizsi vychod tak daleko, ze bych uz nemusel stihnout tam dojit, spechej tam!
  (let (
        (ZbyvaTahu (- (slot_maximum_tahu Parametry) (slot_cislo_tahu Parametry)))
        (NejblizsiVychod (NAJDI-NEJBLIZSI Self 'vychod))
       )
    (when (null NejblizsiVychod)
      (setf NejblizsiVychod (NAJDI-NEJBLIZSI Self nil))
      (when NejblizsiVychod
        (setf (car NejblizsiVychod) (* *TBCNoExitTimeCoeff* (car NejblizsiVychod))))
    )

    (when (and NejblizsiVychod (<= ZbyvaTahu (* *TBCMinTime* (car NejblizsiVychod))))
      ;(format t "HEUR-SPECHEJ! Zbyvajicich tahu: ~d, Zbyvajici vzdalenost: ~d~%" ZbyvaTahu (car NejblizsiVychod))
      (make-instance 'trida_tah :akce (CISLO-NA-SMER (mod (- (cdr NejblizsiVychod) (SMER-NA-CISLO (GET-ORIENTACE-MAPY (getMapa Self)))) 4)))
) ) )

(defmethod VEKTOR-NA-SMER-LETU ((vektor trida_pozice))
  (cond ((rovnost vektor vektor_dolu) 'dozadu)
        ((rovnost vektor vektor_nahoru) 'dopredu)
        ((rovnost vektor vektor_doleva) 'doleva)
        ((rovnost vektor vektor_doprava) 'doprava)
) )

(defmethod HEUR-STRILEJ ((Self Telcontar) (Parametry trida_parametry_tahu))
  ;; Heuristika: pokud sousedis s nestvurou, vystrel na ni
  (when (plusp (slot_pocet_strel (slot_vlastni_postava Parametry)))
    (let (
          (PozicePriser (mapcar #'slot_pozice (slot_viditelne_prisery Parametry)))
         )
      (setf PozicePriser (remove-if (lambda (Pozice) (> (+ (abs (slot_x Pozice)) (slot_y Pozice)) 1)) PozicePriser))
      (when PozicePriser
        ;(format t "Die, bastard! (~d;~d => ~g)~%" (slot_x (car PozicePriser)) (slot_y (car PozicePriser)) (VEKTOR-NA-SMER-LETU (car PozicePriser)))
        (make-instance 'trida_tah :akce 'strela :tahovy_objekt (make-instance 'Sidewinder :smer (VEKTOR-NA-SMER-LETU (car PozicePriser))))
) ) ) )

(defmethod HEUR-AMRAAM ((Self Telcontar) (Parametry trida_parametry_tahu))
  ;; Heuristika: pokud je rovne pred tebou nestvura, posli za ni AMRAAM, nikdy nestrilej vic, nez jednu strelu najednou
  (when (and (zerop (length (slot_viditelne_strely Parametry)))
             (> (slot_pocet_strel (slot_vlastni_postava Parametry)) *TBCMinMissilesLongShoot*))
    (let (
	  (Prisery (slot_viditelne_prisery Parametry))
	 )
      (setf Prisery (remove-if (lambda (Prisera) (or (not (zerop (slot_x (slot_pozice Prisera)))) (> (slot_y (slot_pozice Prisera)) max_dostrel))) Prisery))
      (when Prisery
        ;(format t "Eat this, bastard! (~g at ~d;~d)~%" (slot_x (slot_pozice (car Prisery))) (slot_y (slot_pozice (car Prisery))) (id (slot_tahovy_objekt (car Prisery))))
	(make-instance 'trida_tah :akce 'strela :tahovy_objekt (make-instance 'AMRAAM :cil (id (slot_tahovy_objekt (car Prisery)))))
) ) ) )

(defmethod HEUR-KLICE ((Self Telcontar) (Parametry trida_parametry_tahu))
  ;; Heuristika: Jdi k nejblizsimu znamemu klici
  (declare (ignore Parametry))
  ;(format t "HEUR-KLICE: ~g~%" (cdr (NAJDI-NEJBLIZSI Self 'klic)))
  (cdr (NAJDI-NEJBLIZSI Self 'klic))
)

(defmethod HEUR-STRELY ((Self Telcontar) (Parametry trida_parametry_tahu))
  ;; Heuristika: Jdi k nejblizsi strele, pokud jich uz nemas dost.
  (let ((MamStrel (slot_pocet_strel (slot_vlastni_postava Parametry))))
    (when (< MamStrel *TBCEnoughMissiles*)
      ;(format t "HEUR-STRELY: ~g~%" (cdr (NAJDI-NEJBLIZSI Self 'strela)))
      (if (< MamStrel *TBCMustHaveMissiles*) (cons (cdr (NAJDI-NEJBLIZSI Self 'strela)) 1000)
                                             (cdr (NAJDI-NEJBLIZSI Self 'strela))
) ) ) )

(defmethod HEUR-PRUZKUM ((Self Telcontar) (Parametry trida_parametry_tahu))
  ;; Heuristika: Jdi k nejblizsimu neznamemu policku
  (declare (ignore Parametry))
  ;(format t "HEUR-PRUZKUM: ~g~%" (cdr (NAJDI-NEJBLIZSI Self nil)))
  (cdr (NAJDI-NEJBLIZSI Self nil))
)

(defmethod HEUR-VYCHOD ((Self Telcontar) (Parametry trida_parametry_tahu))
  ;; Heuristika: Jdi k nejblizsimu vychodu
  (declare (ignore Parametry))
  ;(format t "HEUR-VYCHOD: ~g~%" (cdr (NAJDI-NEJBLIZSI Self 'vychod)))
  (cdr (NAJDI-NEJBLIZSI Self 'vychod))
)

; Heuristiky, podle kterych se hodnoti. Seznam obsahuje tecka dvojice ( funkce . hodnota ), pokud je hodnota
; nil, je mineno nekonecno, tzn. to, co funkce vrati, je okamzite prijato jako vysledek (pokud to neni nil).
; Heuristicke funkce vraci BUD: cislo 0--4, znacici 0=dopredu, ..., 3=doleva, 4=stop, nebo nil, kdyz nic nedoporucuji,
; nebo instanci trida_tah. V takovem pripade je brano, jako by meli nekonecnou hodnotu, a objekt je okamzite vracen.
(defparameter *TBCHeuristics* '(
                                (HEUR-SPECHEJ . nil)
                                (HEUR-STRILEJ . nil)
				(HEUR-AMRAAM  . nil)
                                (HEUR-KLICE   .  50)
                                (HEUR-STRELY  .   1)
                                (HEUR-PRUZKUM .  30)
                                (HEUR-VYCHOD  .  10)
)                              )

(defmethod tahova_funkce((Self Telcontar)
                         (Parametry trida_parametry_tahu))

;(time (progn

  (format t "Tah c. ~d: strel: ~d, zivotu: ~d, bodu: ~d~%" (slot_cislo_tahu Parametry) (slot_pocet_strel (slot_vlastni_postava Parametry)) (slot_pocet_zivotu (slot_vlastni_postava Parametry)) (slot_pocet_bodu (slot_vlastni_postava Parametry)))

  ; 1. pouzij dodanou viditelnou mapu
  (POUZIJ-PRIJATOU-MAPU Self (slot_viditelna_mapa Parametry))

  (let (
        (VybranyTah)
        (VybranaAkce)
        (SoucasnySmer (SMER-NA-CISLO (GET-ORIENTACE-MAPY (getMapa Self))))
        (ZvolenySmer)
        (Hlasu (make-array 5 :element-type 'integer :initial-element 0))
       )

    ; 2. rozhodni se, co chces delat
    (setf VybranaAkce
      (dolist (Heur *TBCHeuristics*)
        (setf ZvolenySmer (funcall (car Heur) Self Parametry))
        (when ZvolenySmer
          ;(format t "~g doporucuje ~g (priorita ~g)~%" (car Heur) ZvolenySmer (cdr Heur))
          (when (typep ZvolenySmer 'trida_tah) (return ZvolenySmer))
	  (if (consp ZvolenySmer) (incf (elt Hlasu (car ZvolenySmer)) (cdr ZvolenySmer))
                                  (incf (elt Hlasu ZvolenySmer) (cdr Heur))
	  )
          ; V soucasne verzi ohodnoceni nema smysl hledat dalsi napad, berem a jdem...
          ;(return)
    ) ) )

    (unless VybranaAkce
      ;(format t "Hlasovani dopadlo takto: [~d ~d ~d ~d ~d]~%" (elt Hlasu 0) (elt Hlasu 1) (elt Hlasu 2) (elt Hlasu 3) (elt Hlasu 4))

      (if (every #'zerop Hlasu)
          (progn
               ; NEVIM CO MAM DELAT!!! Nikdo nema jediny navrh...
               (format t "---------- Telcontar by se nejradeji zastrelil... ----------~%")
               (setf ZvolenySmer (random 4))
          )
          (setf ZvolenySmer (position (reduce #'max Hlasu) Hlasu))
      )

      (if (= ZvolenySmer 4)
          (setf VybranaAkce 'stop)
          (when ZvolenySmer
            (if (= ZvolenySmer SoucasnySmer)
              (setf VybranaAkce 'dopredu)
              (setf VybranaAkce (AKCE-PRO-OTOCENI SoucasnySmer ZvolenySmer))
       ) ) )
      ;(format t "OK, chteli bychom na ~g, ted mame orientaci ~g, provedeme ~g~%" ZvolenySmer SoucasnySmer VybranaAkce)
    )

    (if (typep VybranaAkce 'trida_tah)
      (setf VybranyTah VybranaAkce)
      (setf VybranyTah (make-instance 'trida_tah :akce VybranaAkce))
    )

    (when (DELAM-KROK-DO-STRELY VybranyTah Self Parametry)
      (setf (slot_akce VybranyTah) 'stop)
    )

    (when (DELAM-KROK-K-NESTVURE VybranyTah Self Parametry)
      (if (>= (slot_pocet_strel (slot_vlastni_postava Parametry)) *TBCMinMissilesLongerShoot*)
          (setf (slot_akce VybranyTah) 'strela)
          (setf (slot_akce VybranyTah) 'stop)
    ) )

    (COMMENT >PRAVIDLA<
             1. pokud zbyva mene nez x%[TBCMinTime] poctu tahu nutnych k dojiti do nejblizsiho vychodu - jdi tam hned
             2. pokud stojis tesne u nestvury a mas strely - vystrel
             3. pokud vis o nejakem klici - jdi k nejblizsimu
             4. pokud vis o nejake strele a nemas jich vic nez pet - jdi pro nejblizsi
             5. pokud vis o neznamem poli - jdi k nejblizsimu
             6. pokud vis o vychodu - jdi k nejblizsimu
             7. spachej sebevrazdu

             >PLUS KONTROLY<
             0. pokud bys vkrocil do strely - stop!
             1. pokud by te tah privedl do nestvury nebo na jedno policko od ni - pak
                1a. pokud mas nejmene dve strely - vystrel prislusnym smerem
                1b. jinak udelej stop
             2. veskery normalni pohyb je POUZE dopredu - pred pohybem vlevo se provede otoc_doleva...atd.
    )

    ; 3. proved to
    (PROVED-VYBRANY-TAH Self VybranyTah)

    ;(format t "Vybrany Tah: ~g (~g)~%" (slot_akce VybranyTah) VybranyTah)

    VybranyTah
;))
) )
