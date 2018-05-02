(defpackage :grupo01pareja081QA0305            ; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala)                 ; y mancala, y exporta la función de evaluación
  (:export :heuristica :*alias*))              ; heurística y un alias para el torneo

(in-package grupo01pareja081QA0305)

(defvar *alias* '|Triunfito Triunfando|) ; alias que aparecerá en el ranking

(defun heuristica (estado) 
  (if (juego-terminado-p estado)
      (if (< (get-pts (estado-lado-sgte-jugador estado))
             (get-pts (lado-contrario (estado-lado-sgte-jugador estado))))
          -100
        100)
    (heuristica-aux estado)))
  
(defun heuristica-aux (estado)
  (-
   (* (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 6) 8) 
   (- (* (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 5) 4) 5)
   (- (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 4) 3.2)
   (* (length (posiciones-con-fichas-lado (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) 0)) 4)
   (- (* (length (posiciones-con-fichas-lado (estado-tablero estado) (estado-lado-sgte-jugador estado) 0)) 4))
   (cuenta-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 0)
   (- (cuenta-fichas (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) 0))
   (get-fichas (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)) 6)))