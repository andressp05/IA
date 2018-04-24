;;; MODELO FICHERO JUGADOR PARA SUBIR
;;; Cambiar el defpackage, el in-package y el NOMBREALIAS

(defpackage :grupo01pareja082QA2104            ; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala)                 ; y mancala, y exporta la función de evaluación
  (:export :heuristica :*alias*))              ; heurística y un alias para el torneo

(in-package grupo01pareja082QA1704)

(defvar *alias* '|NOMBREALIAS|) ; alias que aparecerá en el ranking

;Heuristica a implementar
(defun heuristica (estado)
  )

;Funciones Auxiliares
(defun ...)
