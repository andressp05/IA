(defpackage :grupo01pareja081QA1904            ; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala)                 ; y mancala, y exporta la funci�n de evaluaci�n
  (:export :heuristica :*alias*))              ; heur�stica y un alias para el torneo

(in-package grupo01pareja081QA1904)

(defvar *alias* '|Cepeda sigue a mi Sis en Insta|) ; alias que aparecer� en el ranking

(defun heuristica (estado)
  (+ (our-pesc-map (list-lado estado (estado-lado-sgte-jugador estado))
                   '(0 1 1 2 3 5))
     (our-pesc-map (list-lado estado (lado-contrario (estado-lado-sgte-jugador estado)))
                   '(5 3 2 1 1 0))))
  
(defun our-pesc-map (lista1 lista2)
  (reduce '+ (mapcar #'* lista1 lista2)))