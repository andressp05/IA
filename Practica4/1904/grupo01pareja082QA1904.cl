(defpackage :grupo01pareja082QA1904            ; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala)                 ; y mancala, y exporta la función de evaluación
  (:export :heuristica :*alias*))              ; heurística y un alias para el torneo

(in-package grupo01pareja082QA1904)

(defvar *alias* '|Alfred no sabe hablar espanyol|) ; alias que aparecerá en el ranking

(defun heuristica (estado)
  (+ (our-pesc-map (list-lado estado (estado-lado-sgte-jugador estado))
                   '(0 1 4 9 16 25))
     (our-pesc-map (list-lado estado (lado-contrario (estado-lado-sgte-jugador estado)))
                   '(25 16 9 4 1 0))))
  
(defun our-pesc-map (lista1 lista2)
  (reduce '+ (mapcar #'* lista1 lista2)))