(in-package "BLUDISTE")
(provide "IRS")
(require "konstanty" "konstanty")
(require "smer" "smer")
(require "pozice" "pozice")
(require "mapa" "mapa")
(require "tah" "tah")
(require "utils" "utils")
(require "aktivni_objekt" "aktivni_objekt")

;;; **********************************
;;; IRS -- Inercialni navigacni system
;;; **********************************

;;; Timhle se da prepnout, jestli se pracuje se seznamy, nebo s poli
(defparameter *MAP-TYPE* 'vector)
(defparameter *ROUTES-TYPE* 'vector)

(defun OTOC-MAPOVE-POLE (Pole)
  ;; otoci seznam, resp. jiny druh sekvence. Pro pouziti viz OTOC-MAPU
  (etypecase Pole
    (list    (nreverse (apply #'mapcar (cons #'list Pole))))
    (vector  (nreverse (apply #'map 'vector (cons #'list (concatenate 'list Pole)))))
) )

(defmethod MAX-POZICE ((A trida_pozice) (B trida_pozice))
  ;; Vyrobi instanci trida_pozice s maximalnimi souradnicemi v obou slozkach
  (make-instance 'trida_pozice :x (max (slot_x A) (slot_x B)) :y (max (slot_y A) (slot_y B)))
)

(defclass MapData ()
  (
   (pole :initarg :pole :initform () #|:type *MAP-TYPE*|# :accessor GET-MAPA)                      ; Mapa: ((...) (...) ...)
   (velikost :initarg :velikost :initform (make-instance 'trida_pozice) :type trida_pozice :accessor GET-VELIKOST-MAPY) ; velikost mapy
   (pozice :initarg :pozice :initform (make-instance 'trida_pozice) :type trida_pozice :accessor GET-POZICE-NA-MAPE) ; pozice na mape
   (smer :initarg :smer :initform 'nahoru :type symbol :accessor GET-ORIENTACE-MAPY)                                 ; smer pohledu
  )
  (:documentation "Trida pro ulozeni informaci o mape a polohy na ni")
)

(comment
(defmethod POLICKO ((Mapa MapData) (Pozice trida_pozice))
  ;; Vrati policko na pozici Pozice v mape Mapa (pokud odkazuje mimo, vrati ZED)
  (let ((Velikost (GET-VELIKOST-MAPY Mapa)))
    (if (or (minusp (slot_x Pozice))
            (minusp (slot_y Pozice))
            (>= (slot_x Pozice) (slot_x Velikost))
            (>= (slot_y Pozice) (slot_y Velikost Mapa))
        ) 'zed
          (elt (elt (GET-MAPA Mapa) (slot_y Pozice)) (slot_x Pozice))
) ) )
)

(defmethod ORIENTUJ-MAPU ((Mapa MapData))
  ;; Zorientuje mapu (tak, aby pseudosever byl nahore).
  (let ((Smer (SMER-NA-CISLO (slot-value Mapa 'smer))))
    (if (zerop smer)
      Mapa                     ; bud je mapa uz zorientovana
      (dotimes (i smer Mapa)   ; nebo ji x-krat otoc
        (OTOC-MAPU Mapa)
      )
) ) )

(defmethod OTOC-MAPU ((Mapa MapData))
  ;; Otoci mapu o 90 proti smeru hodinovych rucicek (Sever->Zapad)
  (let*
    (  (OldSize (GET-VELIKOST-MAPY Mapa))
       (OldPos  (GET-POZICE-NA-MAPE Mapa))
       (OldDir  (GET-ORIENTACE-MAPY Mapa))
       (OldMap  (GET-MAPA Mapa))
       (NewSize (make-instance 'trida_pozice :x (slot_y OldSize) :y (slot_x OldSize)))
       (NewPos  (make-instance 'trida_pozice :x (slot_y OldPos)  :y (- (slot_y NewSize) (slot_x OldPos) 1)))
       (NewDir  (SMER-DOLEVA OldDir))
       (NewMap  (OTOC-MAPOVE-POLE OldMap))
    )
    (setf (GET-MAPA Mapa) NewMap)
    (setf (GET-VELIKOST-MAPY Mapa) NewSize)
    (setf (GET-POZICE-NA-MAPE Mapa) NewPos)
    (setf (GET-ORIENTACE-MAPY Mapa) NewDir)
) )

(defun DO-MAP-ROW-UPDATE (CurrInfoRow NewInfoRow)
  ;; Provede update jednoho radku mapy CurrInfoRow tak, ze kazde ne-nil policko NewInfoRow zkopiruje do CurrInfoRow.
  (map *MAP-TYPE* (lambda (CurrCell NewCell) (if NewCell NewCell CurrCell)) CurrInfoRow NewInfoRow)
)

(defmethod DO-MAP-UPDATE ((CurrMap MapData) (NewInfo MapData))
  ;; Provede update mapy CurrMap tak, ze kazde ne-nil policko NewInfo zkopiruje do CurrMap.
  ;; Ocekava obe mapy orientovane, o stejne velikosti.
  (setf (GET-MAPA CurrMap) (map *MAP-TYPE* #'DO-MAP-ROW-UPDATE (GET-MAPA CurrMap) (GET-MAPA NewInfo)))
)

(defclass MapaIRS (MapData)
  (
   (smerovani :initarg :smerovani :initform () #|:type *ROUTES-TYPE*|#) ; Jako Mapa, obsahuje polozky (Vzdalenost . Smer) (smer je k hraci)
  )
  (:documentation "Trida pro inercialni navigacni system IRS")
)

(defmethod OBAL-PRIJATOU-MAPU ((ViditelnaMapa trida_mapa) Smer)
   ;; Vytvori instanci tridy MapaIRS pro prijatou mapu
   (make-instance 'MapaIRS
                  :pole     (slot_pole ViditelnaMapa) ;(VECTOR-TO-TYPE *MAP-TYPE* (slot_pole ViditelnaMapa))
                  :velikost (make-instance 'trida_pozice :x (+ 1 max_viditelnost max_viditelnost) :y (1+ max_viditelnost))
                  :pozice   (make-instance 'trida_pozice :x max_viditelnost :y 0)
                  :smer     Smer
)  )

(defun PLATNE-SOURADNICE (Coords Mapa)
  ;; Vraci, zda Coords (ve forme (X . Y)) obsahuji platne souradnice pro Mapa
  (not (or (minusp (car Coords))
           (minusp (cdr Coords))
           (>= (car Coords) (slot_x (GET-VELIKOST-MAPY Mapa)))
           (>= (cdr Coords) (slot_y (GET-VELIKOST-MAPY Mapa)))
) )    )

(defun PRES-JE-LEPSI (Coords Smerovani Mapa Policko Soucasne)
  ;; Vraci nove hodnoceni, pokud je lepsi smerovat pres Coords (smerem Smer z Coords) nez se soucasnym skore (Soucasne), jinak vraci nil
  (declare (ignore Policko))
  (when (PLATNE-SOURADNICE Coords Mapa)
    (let* (
           (StarePres (elt (elt Smerovani (cdr Coords)) (car Coords)))
           (PresPole  (elt (elt (GET-MAPA Mapa) (cdr Coords)) (car Coords)))
           (PresHodn  (car StarePres))
          )
      (when (and
              StarePres
              (not (position PresPole '(zed vychod nil)))     ; nelze smerovat skrz zed, vychod, nezname pole
              (or (null Soucasne)
                  (< (1+ PresHodn) (car Soucasne))
            ) )
        (1+ PresHodn)
) ) ) )

(defmethod UPDATE-SMEROVANI (Smerovani (Mapa MapData))
  ;; Provede jeden krok updatu smerovani, vraci nil pokud bylo smerovani upraveno, t jinak
  (let (
        (Zmena t)
        (RadekSmerovani)
        (XY)
        (Policko)
        (Nove)
        (Velikost (GET-VELIKOST-MAPY Mapa))
        ;(Pozice   GET-POZICE-NA-MAPE Mapa)
       )
    (dotimes (Y (slot_y Velikost))
      (setf RadekSmerovani (elt Smerovani Y))
      (dotimes (X (slot_x Velikost))
        (setf XY (cons X Y))
        (setf Policko (elt (elt (GET-MAPA Mapa) Y) X))
        (unless (eq Policko 'zed)                          ; do zdi se nesmeruje
          (dolist (Smer '(0 1 2 3))
            (setf Nove (PRES-JE-LEPSI (+COORDS XY (SMER-DELTA Smer)) Smerovani Mapa Policko (elt RadekSmerovani X)))
            (when Nove
              (setf Zmena nil)
              (setf (elt RadekSmerovani X) (cons Nove Smer))
    ) ) ) ) )
    (setf (slot-value Mapa 'smerovani) Smerovani)
    Zmena
) )

(defun NILUJ-ZDI-RADEK (Smerovani Mapa)
  ;; Pomocna funkce pro jeden radek -- viz NILUJ-ZDI
  (map *ROUTES-TYPE* (lambda (Smerovani Pole) (unless (eq Pole 'zed) Smerovani)) Smerovani Mapa)
)

(defun NILUJ-ZDI (Smerovani Mapa)
  ;; Vrati Smerovani, ve kterem jsou ovsem nil vsude tam, kde jsou v mape zdi
  (map *ROUTES-TYPE* #'NILUJ-ZDI-RADEK Smerovani Mapa)
)

(defmethod PREPOCITEJ-SMEROVANI ((Self MapaIRS))
  ;; Prepocita pole smerovani
  (with-slots (velikost pole pozice) Self
    (let (
          (Smerovani   (NILUJ-ZDI (slot-value Self 'smerovani) (GET-MAPA Self)))
         )
      (setf (elt (elt Smerovani (slot_y pozice)) (slot_x pozice)) (cons 0 nil))

      (loop
        (if (UPDATE-SMEROVANI Smerovani Self) (return))
      )
      (setf (slot-value Self 'smerovani) Smerovani)
) ) )

(defun SMEROVANI+1 (RadekSmerovani)
  ;; Pro jeden radek smerovani ((vzdal . smer) (vzdal . smer) ...) zvedne vsechny vzdalenosti o jedna, tam kde je (0), da nil
  (map *ROUTES-TYPE* (lambda (Polozka) (when (and Polozka (plusp (car Polozka))) (cons (1+ (car Polozka)) (cdr Polozka)))) RadekSmerovani)
)

(defmethod ROZTAHNI-MAPU ((Mapa MapaIRS) (NewPos trida_pozice) (NewSize trida_pozice) &optional PouzijSmerovani)
  ;; Roztahne mapu na velikost NewSize tak, aby v ni hrac byl na pozici NewPos.
  ;; Ocekava orientovanou mapu.
  (let*
    (  (OldSize   (GET-VELIKOST-MAPY Mapa))
       (OldPos    (GET-POZICE-NA-MAPE Mapa))
       (AddPos    (rozdil NewPos OldPos))
       (AddBottom (rozdil (rozdil NewSize OldSize) AddPos))
       (OldMap    (GET-MAPA Mapa))
       (NewMap    (concatenate *MAP-TYPE*   ; Nova mapa vznikne jako slozenina tri casti:
                     (MAKE-MATRIX (slot_x NewSize) (slot_y AddPos) *MAP-TYPE*)        ; 1. Nove prazdne horni radky
                     (map *MAP-TYPE* (lambda (Radek) (concatenate *MAP-TYPE* (make-sequence *MAP-TYPE* (slot_x AddPos)) Radek (make-sequence *MAP-TYPE* (slot_x AddBottom)))) OldMap)                           ; 2. Roztazene puvodni radky
                     (MAKE-MATRIX (slot_x NewSize) (slot_y AddBottom) *MAP-TYPE*)     ; 3. Nove prazdne dolni radky
                  ))
       (OldRoutes (slot-value Mapa 'smerovani))
       (NewRoutes (when PouzijSmerovani
                    (concatenate *ROUTES-TYPE*
                       (MAKE-MATRIX (slot_x NewSize) (slot_y AddPos) *ROUTES-TYPE*)
                       (map *ROUTES-TYPE* (lambda (Radek) (concatenate *ROUTES-TYPE* (make-sequence *ROUTES-TYPE* (slot_x AddPos)) (SMEROVANI+1 Radek) (make-sequence *ROUTES-TYPE* (slot_x AddBottom)))) OldRoutes)
                       (MAKE-MATRIX (slot_x NewSize) (slot_y AddBottom) *ROUTES-TYPE*)
                  )))
    )
    (when PouzijSmerovani
      (setf (slot-value Mapa 'smerovani) NewRoutes)
    )
    (setf (GET-VELIKOST-MAPY Mapa) NewSize)
    (setf (GET-POZICE-NA-MAPE Mapa) NewPos)
    (setf (GET-MAPA Mapa) NewMap)
    Mapa
) )

(defun VYPIS-POLE-MAPY (Mapa)
  (princ 
    (case Mapa
      (chodba #\.)
      (zed    #\#)
      (klic   #\k)
      (strela #\|)
      (vychod #\O)
      (t      (if (null Mapa) #\? Mapa))
  ) )
)

(defconstant SMERY-IKONKY '(#\^ #\> #\V #\<))

(defun VYPIS-POLE-SMEROVANI (Smerovani)
  (if Smerovani (format t "~3d ~d " (car Smerovani) (elt SMERY-IKONKY (NIL-TO-ZERO (cdr Smerovani))))
                (format t "##### "))
)

(defun VYPIS-RADEK-MAPY (Mapa)
  (map nil #'VYPIS-POLE-MAPY Mapa)
  (terpri)
)

(defun VYPIS-RADEK-SMEROVANI (Smerovani)
  (map nil #'VYPIS-POLE-SMEROVANI Smerovani)
  (terpri)
)

(defmethod DEBUG-VYPIS ((Mapa MapaIRS) &optional (SeSmerovanim nil))
  ;; ladici vypis objektu MapaIRS

  (format t "IRS Information: Orientation=~d, Map size=~dx~d, Current pos=[~d;~d]~%" (GET-ORIENTACE-MAPY Mapa) (slot_x (GET-VELIKOST-MAPY Mapa)) (slot_y (GET-VELIKOST-MAPY Mapa)) (slot_x (GET-POZICE-NA-MAPE Mapa)) (slot_y (GET-POZICE-NA-MAPE Mapa)))

  (with-slots (Pole Smerovani) Mapa
    (map nil #'VYPIS-RADEK-MAPY (reverse Pole))
    (if SeSmerovanim (map nil #'VYPIS-RADEK-SMEROVANI (reverse Smerovani)))
  )

  nil
)

(defmethod POUZIJ-MAPU ((Mapa MapaIRS) (ViditelnaMapa trida_mapa))
  ;; Updatuje mapu informace z prijate mapy Mapa, Smer je smer pohledu postavy
  (let* (  (Neorientovana (OBAL-PRIJATOU-MAPU ViditelnaMapa (GET-ORIENTACE-MAPY Mapa)))
           (PrijataData   (ORIENTUJ-MAPU Neorientovana))
           (NovaPozice    (MAX-POZICE (GET-POZICE-NA-MAPE Mapa) (GET-POZICE-NA-MAPE PrijataData)))
           (NovyZbytek    (MAX-POZICE (rozdil (GET-VELIKOST-MAPY Mapa) (GET-POZICE-NA-MAPE Mapa))
                                      (rozdil (GET-VELIKOST-MAPY PrijataData) (GET-POZICE-NA-MAPE PrijataData))))
           (NovaVelikost  (soucet NovaPozice NovyZbytek))
        )

        ; Debug
        ;(format t "~%----------~%")
        ;(format t "Puvodne zapamatovana mapa:~%")
        ;(DEBUG-VYPIS Mapa)
        ;(format t "Prijata mapa:~%")
        ;(DEBUG-VYPIS PrijataData)

        ; zpracuj prijate informace
        (ROZTAHNI-MAPU PrijataData NovaPozice NovaVelikost)
        ;(format t "Roztazena prijata data:~%")
        ;(DEBUG-VYPIS PrijataData)
        (ROZTAHNI-MAPU Mapa NovaPozice NovaVelikost t)
        ;(format t "Roztazena zapamatovana mapa:~%")
        ;(DEBUG-VYPIS Mapa)
        (DO-MAP-UPDATE Mapa PrijataData)
        (PREPOCITEJ-SMEROVANI Mapa)

        ; Debug
        ;(format t "Nove zapamatovana mapa:~%")
        ;(DEBUG-VYPIS Mapa t)
) )

(defmethod nova_pozice((postava MapaIRS) (smer symbol))
  ;; Totez jako aktivni_objekt\nova_pozice, jenom pouziva MapaIRS misto aktivni_objekt
  (let* ((pozice (GET-POZICE-NA-MAPE postava))
         (nova_pozice_relativne (nova_pozice_relativne smer))
         (nova_pozice_relativne_transformovana
           (transformace_pohled nova_pozice_relativne
                                (GET-ORIENTACE-MAPY postava)))
        )

    ; Nekde po ceste doslo k rozdilu mezi mymi souradnicemi a temi v aktivni_objekt. Takze tenhle hack to snad spravi...
    (setf (slot_y nova_pozice_relativne_transformovana) (- (slot_y nova_pozice_relativne_transformovana)))

    (soucet pozice
            nova_pozice_relativne_transformovana)
  )
)

(defmethod PROVEDEN-TAH ((Self MapaIRS) (Tah trida_tah))
  ;; Zmeni ulozene hodnoty pote, co byl proveden Tah
  ;(format t "Pred PROVEDEN-TAH: Orientace=~d, Pozice [~d;~d], Tah=~g~%" (GET-ORIENTACE-MAPY Self) (slot_x (GET-POZICE-NA-MAPE Self)) (slot_y (GET-POZICE-NA-MAPE Self)) (slot_akce Tah))
  (with-slots (pozice smer) Self
    (case (slot_akce Tah)
      ((dopredu dozadu doleva doprava) (setf pozice (nova_pozice Self (slot_akce Tah))))
      (otoc_doprava (setf smer (SMER-DOPRAVA smer)))
      (otoc_doleva  (setf smer (SMER-DOLEVA smer)))
      (otoc_dozadu  (setf smer (SMER-DOZADU smer)))
    )
  )
  ;(format t "Po PROVEDEN-TAH: Orientace=~d, Pozice [~d;~d], Tah=~g~%" (GET-ORIENTACE-MAPY Self) (slot_x (GET-POZICE-NA-MAPE Self)) (slot_y (GET-POZICE-NA-MAPE Self)) (slot_akce Tah))
  t
)

