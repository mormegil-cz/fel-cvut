(in-package "BLUDISTE")
(provide "IRS_Debugger")
(require "postava" "postava")
(require "tahovy_objekt" "tahovy_objekt")
(require "parametry_tahu" "parametry_tahu")
(require "tah_graficky" "tah_graficky")
(require "IRS" "IRS")
(require "utils" "utils")

;;; Tento soubor obsahuje funkce pro postavicku rizenou rucne, grafickym rozhranim, ktera ovsem pracuje s IRS.
;;;   tato postavicka slouzi jako debugger pro IRS.

(defclass IRS_Debugger (trida_tahovy_objekt)
  (
   (mapa :initarg :mapa :accessor getMapa :initform (make-instance 'MapaIRS) :type MapaIRS)      ; IRS
   (rozhrani :initarg :rozhrani :accessor getRozhrani :initform (make-instance 'trida_tahovy_objekt_postava_graficky) :type trida_tahovy_objekt_postava_graficky)
  )
  (:documentation "Mozek postavicky Telcontar")
)

(defmethod POUZIJ-PRIJATOU-MAPU ((Self IRS_Debugger) (PrijataMapa trida_mapa))
  (POUZIJ-MAPU (getMapa Self) PrijataMapa)
)

(defmethod PROVED-VYBRANY-TAH ((Self IRS_Debugger) (Tah trida_tah))
  (PROVEDEN-TAH (getMapa Self) Tah)
)

(defmethod start ((Self IRS_Debugger))
  (start (getRozhrani Self))
)

(defmethod konec ((Self IRS_Debugger))
  (konec (getRozhrani Self))
)

(defmethod tahova_funkce((Self IRS_Debugger)
                         (Parametry trida_parametry_tahu))

  (POUZIJ-PRIJATOU-MAPU Self (slot_viditelna_mapa Parametry))
  (let ((VybranyTah (tahova_funkce (getRozhrani Self) Parametry)))
    (PROVED-VYBRANY-TAH Self VybranyTah)
    VybranyTah
) )
