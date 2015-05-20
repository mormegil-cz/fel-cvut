(in-package "BLUDISTE")

(provide "AMRAAM")
(require "tah" "tah")
(require "tahovy_objekt" "tahovy_objekt")
(require "parametry_tahu" "parametry_tahu")
(require "aktivni_objekt")

;;; Tento soubor obsahuje definici tridy AMRAAM, coz je strela, ktera pronasleduje danou priseru

(defclass AMRAAM (trida_tahovy_objekt)
  (
   (cil :initarg :cil :accessor getCil :type symbol)
   (lastMove :initarg :lastMove :accessor getLastMove :initform 'dopredu :type symbol)
  )
  (:documentation "Trida strely typu AMRAAM")
)

(defmethod tahova_funkce((Self AMRAAM)
                         (Parametry trida_parametry_tahu))

  (let* (
	 (Tah (make-instance 'trida_tah :akce (getLastMove Self)))
         (CilID (getCil Self))
         (CilSeznam (remove-if-not (lambda (Prisera) (eq (id (slot_tahovy_objekt Prisera)) CilID)) (slot_viditelne_prisery Parametry)))
	 (CilPrisera (car CilSeznam))
        )
    (when CilPrisera
      (cond
        ((plusp (slot_y (slot_pozice CilPrisera))) (setf (slot_akce Tah) 'dopredu))
	((plusp (slot_x (slot_pozice CilPrisera))) (setf (slot_akce Tah) 'doprava))
	((minusp (slot_x (slot_pozice CilPrisera))) (setf (slot_akce Tah) 'doleva))
    ) )
    (setf (getLastMove Self) (slot_akce Tah))
    Tah
) )
