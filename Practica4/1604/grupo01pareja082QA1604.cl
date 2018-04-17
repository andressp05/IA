(defpackage :grupo01pareja082QA1604            ; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala)                 ; y mancala, y exporta la función de evaluación
  (:export :heuristica :*alias*))              ; heurística y un alias para el torneo


(in-package grupo01pareja082QA1604)

(defun heuristica (estado)
  (+ (heuristica-aux estado) (casillas-nulas (estado-tablero estado) (estado-lado-sgte-jugador estado) 5)))

(defvar *alias* '|Riosal2.0|) ; alias que aparecerá en el ranking

(defun heuristica-aux (estado)
  (+
   (* 6 (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 0))
   (* 5 (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 1))
   (* 4 (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 2))
   (* -3 (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 3))   
   (* -4 (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 4))
   (* -5 (get-fichas (estado-tablero estado) (estado-lado-sgte-jugador estado) 5))))

(defun casillas-nulas (tablero lado pos)   ; funciones auxiliares usadas por heurística
  (cond 
   ((< pos 3) 0)
   ((equal 0 (get-fichas tablero lado pos)) (+ 1 (casillas-nulas tablero lado (- pos 1))))
   (t (casillas-nulas tablero lado (- pos 1)))))
