(defpackage :grupo01pareja082QA1704            ; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala)                 ; y mancala, y exporta la función de evaluación
  (:export :heuristica :*alias*))              ; heurística y un alias para el torneo

(in-package grupo01pareja081QA1704)

(defvar *alias* '|AitanaWar|) ; alias que aparecerá en el ranking

(defun heuristica (estado)
  (+ (our-pesc-map (list-lado estado (estado-lado-sgte-jugador estado))
                   '(0 1 2 3 4 5))
     (our-pesc-map (list-lado estado (lado-contrario (estado-lado-sgte-jugador estado)))
                '(5 4 3 2 1 0))))

(defun our-pesc-map (lista1 lista2)
  (reduce '+ (mapcar #'* lista1 lista2)))