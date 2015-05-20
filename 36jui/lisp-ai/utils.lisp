(in-package "BLUDISTE")
(provide "utils")

;;; Nektere zajimave pomocne funkce, co se jinam neveslo...

(defconstant *SMERY* '(nahoru doprava dolu doleva))

(defconstant *MOVE-DELTAs* #(( 0 . +1)
                             (+1 .  0)
                             ( 0 . -1)
                             (-1 .  0)))

(defconstant *OTACECI-AKCE* #(stop otoc_doprava otoc_dozadu otoc_doleva))

;; Secti/odecti po slozkach dva vektory ve forme (X . Y)
(defun +COORDS (C1 C2) (cons (+ (car C1) (car C2)) (+ (cdr C1) (cdr C2))))
(defun -COORDS (C1 C2) (cons (- (car C1) (car C2)) (- (cdr C1) (cdr C2))))

(defun SMER-DELTA (Smer)
  (elt *MOVE-DELTAs* Smer)
)

(defun CISLO-NA-SMER (Cislo)
  (nth Cislo *SMERY*)
)

(defun SMER-NA-CISLO (Smer)
  (position Smer *SMERY*)
)

(defun SMER-DOLEVA (Smer)
  (CISLO-NA-SMER (mod (+ (SMER-NA-CISLO Smer) 3) 4))
)

(defun SMER-DOPRAVA (Smer)
  (CISLO-NA-SMER (mod (+ (SMER-NA-CISLO Smer) 1) 4))
)

(defun SMER-DOZADU (Smer)
  (CISLO-NA-SMER (mod (+ (SMER-NA-CISLO Smer) 2) 4))
)

(defun AKCE-PRO-OTOCENI (SoucasnySmer ZadouciSmer)
  ;; Jakou akci zajistim, aby jsem ze soucasneho smeru byl otocen zadoucim smerem? (Smery zadany ciselne)
  (elt *OTACECI-AKCE* (mod (- ZadouciSmer SoucasnySmer) 4))
)

(defun MAKE-MATRIX (Width Height &optional (type 'list))
  ;; Vyrobi matici (seznam seznamu) o zadanych rozmerech se samymi nil. Napr: W=3 H=1 ==> ((nil nil nil))
  ;; pomoci type lze zvolit, zda to ma byt seznam seznamu, vektor vektoru, atd.
  (map type (lambda (X) (declare (ignore X)) (make-sequence type Width)) (make-sequence type Height))
)

(defun NIL-TO-ZERO (Something)
  ;; Vrati parametr, jenom misto nil vraci nulu.
  (if Something Something
                0)
)

;;; Ruzne prace se sekvencemi (prevody typu jsou fajn, ale pro me zcela nepouzitelne :) )
(defun TYPE-TO-TYPE (Type1 Type2 Original)
  ;; Prevede sekvenci Original typu Type1 na typ Type2, vcetne vnorenych sekvenci
  (map Type2 (lambda (Item)
               (if (typep Item Type1) (TYPE-TO-TYPE Type1 Type2 Item)
                                      Item
             ) )
       Original
) )

(defun VECTOR-TO-TYPE (Type Vec)
  (TYPE-TO-TYPE 'vector Type Vec)
)

(defun VECTOR-TO-LIST (Vec)
  (TYPE-TO-TYPE 'vector 'list Vec)
)

(defun LIST-TO-VECTOR (Lst)
  (TYPE-TO-TYPE 'list 'vector Lst)
)

(defun MAP2 (ResultType Function &rest Lists)
  ;; Obdoba map, ale pro dvouurovnove seznamy ((...)(...)...), vyrobi seznam vysledku map
  ;;   napr: (MAP2 'list #'+ '((1 2 3) (4 5 6) (7 8 9)) '((1 3 5) (7 9 11) (13 15 17))) ==>
  ;;         ((2 5 8) (11 14 17) (20 23 26))

  (apply #'map (append (list ResultType 
                             (lambda (&rest ListsSlice) (apply #'map (append (list ResultType Function) ListsSlice)))
                       )
                Lists)
  )
)

(defmacro COMMENT (&rest comments) (declare (ignore comments)))
