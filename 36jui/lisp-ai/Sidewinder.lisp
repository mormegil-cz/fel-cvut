(in-package "BLUDISTE")

(provide "sidewinder")
(require "tah" "tah")
(require "tahovy_objekt" "tahovy_objekt")
(require "parametry_tahu" "parametry_tahu")

;;; Tento soubor obsahuje definici tridy Sidewinder, coz je primitivni strela, ktera neni nijak navadena, jediny rozdil oproti implicitni strele je, ze leti stale zadanym smerem, ktery nemusi byt nutne dopredu

(defclass Sidewinder (trida_tahovy_objekt)
  (
   (smer :initarg :smer :accessor getSmer :initform 'dopredu :type symbol)
  )
  (:documentation "Trida strely typu Sidewinder")
)

(defmethod tahova_funkce((Self Sidewinder)
                         (Parametry trida_parametry_tahu))
  (declare (ignore Parametry))
  ;(format t "POZOR! Sidewinder leti, smer ~g~%" (getSmer Self))
  (make-instance 'trida_tah :akce (getSmer Self))
)
