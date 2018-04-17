(defpackage :grupo01pareja081QA1604            ; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala)                 ; y mancala, y exporta la función de evaluación
  (:export :heuristica :*alias*))              ; heurística y un alias para el torneo


(in-package grupo01pareja081QA1604)

;Heuristica que da preferencia a las casillas mas cercanas al kahala
(defun heuristica (estado)
  (+
   (* 6 (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 0))
   (* 5 (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 1))
   (* 4 (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 2))
   (* 3 (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 3))   
   (* 2 (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 4))
   (* 1 (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 5))))

(defvar *alias* '|Riosal|) ; alias que aparecerá en el ranking