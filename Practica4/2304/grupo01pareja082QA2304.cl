(defpackage :grupo01pareja082QA2304            ; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala)                 ; y mancala, y exporta la función de evaluación
  (:export :heuristica :*alias*))              ; heurística y un alias para el torneo

(in-package grupo01pareja082QA2304)

(defvar *alias* '|Triunfito|) ; alias que aparecerá en el ranking

;Heuristica a implementar
(defun heuristica (estado) 
    (- (+(get-pts (estado-lado-sgte-jugador estado))
          (*(get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 6) 4)
          )
       (*(get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 5) 2)
       (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 4)
       (*(length (posiciones-con-fichas-lado (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) 0)) 4)
       (- (cuenta-fichas (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) 0))
       (+(get-pts (lado-contrario (estado-lado-sgte-jugador estado))) 
          (get-fichas (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) 6))
       ))