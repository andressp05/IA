;;; MODELO FICHERO JUGADOR PARA SUBIR
;;; Cambiar el defpackage, el in-package y el NOMBREALIAS

(defpackage :grupo01pareja082QA2104            ; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala)                 ; y mancala, y exporta la funci�n de evaluaci�n
  (:export :heuristica :*alias*))              ; heur�stica y un alias para el torneo

(in-package grupo01pareja082QA1704)

(defvar *alias* '|NOMBREALIAS|) ; alias que aparecer� en el ranking

;Heuristica a implementar
(defun heuristica (estado)
  )

;Funciones Auxiliares
(defun ...)
