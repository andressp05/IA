(use-package 'mancala)

;(declaim #+sbcl(sb-ext:muffle-conditions style-warning))

;;; ------------------------------------------------------------------------------------------
;;; FUNCIONES DE DEFINICION DE JUGADORES Y PARTIDAS DE PRUEBA
;;; ------------------------------------------------------------------------------------------

;;; f-juego que utiliza busqueda negamax (con o sin poda)
;;; ------------------------------------------------------------------------------------------
(defun f-j-nmx (estado profundidad-max f-eval)
;;;(negamax-a-b estado profundidad-max f-eval))
  (negamax estado profundidad-max f-eval))

;;; f-juego controlado por un humano
;;; ------------------------------------------------------------------------------------------
;(defun f-j-humano (estado &optional profundidad-max f-eval)
;  (and profundidad-max f-eval)      ; dummy to avoid compiler warnings
;  (setq *verjugada* t)
;  (let ((accion (pide-accion (acciones-posibles estado))))
;    (unless (null accion) (ejecuta-accion estado accion))))

(defvar *jdr-humano* (make-jugador
                        :nombre   '|Humano|
                        :f-juego  #'f-j-humano
                        :f-eval   nil))

(defvar *jdr-human2* (make-jugador
                        :nombre   '|Human2|
                        :f-juego  #'f-j-humano
                        :f-eval   nil))

;;; f-juego para un jugador que realiza movimientos aleatorios
;;; ------------------------------------------------------------------------------------------
(defun f-j-aleatorio (estado &optional profundidad-max f-eval)
  (and profundidad-max f-eval)      ; dummy to avoid compiler warnings
  (let ((lst-acciones (acciones-posibles estado)))
    (ejecuta-accion estado (nth (random (length lst-acciones)) lst-acciones))))

;;; Jugador que no evalua y juega aleatoriamente
;;; ------------------------------------------------------------------------------------------
(defvar *jdr-aleatorio* (make-jugador
                        :nombre   '|Ju-Aleatorio|
                        :f-juego  #'f-j-aleatorio
                        :f-eval   nil))

;;; f-juego para un jugador que siempre juega la primera opcion
;;; ------------------------------------------------------------------------------------------
(defun f-j-1st-opt (estado &optional profundidad-max f-eval)
  (and profundidad-max f-eval)      ; dummy to avoid compiler warnings
  (ejecuta-accion estado (first (acciones-posibles estado))))

;;; Jugador que no evalua y juega la primera opcion disponible
;;; ------------------------------------------------------------------------------------------
(defvar *jdr-1st-opt* (make-jugador
                        :nombre   '|Ju-1st-Opt|
                        :f-juego  #'f-j-1st-opt
                        :f-eval   nil))

;;; f-juego para un jugador que siempre juega la ultima opcion
;;; ----------------------------------------------------------
(defun f-j-last-opt (estado &optional profundidad-max f-eval)
  (and profundidad-max f-eval)      ; dummy to avoid compiler warnings
  (ejecuta-accion estado (car (last (acciones-posibles estado)))))

;;; Jugador que no evalua y juega la segunda opcion disponible (si puede)
;;; ------------------------------------------------------------------------------------------
(defvar *jdr-last-opt* (make-jugador
                        :nombre   '|Ju-Last-Opt|
                        :f-juego  #'f-j-last-opt
                        :f-eval   nil))

;;; Funcion de evaluacion que da al estado recibido una puntuacion aleatoria
;;; ------------------------------------------------------------------------------------------
(defun f-eval-aleatoria (estado)
  (when estado t)                   ; dummy para evitar warnings de compilador
  (random 100) )

;;; f-juego que utiliza busqueda negamax pero evalua aleatoriamente
;;; ------------------------------------------------------------------------------------------
(defvar *jdr-nmx-eval-aleatoria* (make-jugador
                        :nombre   '|Ju-Nmx-Eval-Aleatoria|
                        :f-juego  #'f-j-nmx
                        :f-eval   #'f-eval-aleatoria))

;;; f-juego que aborta
;;; ------------------------------------------------------------------------------------------
(defun f-eval-erronea (estado)
  (when estado t)                   ; dummy para evitar warnings de compilador
  (/ 1 0))

(defvar *jdr-erroneo* (make-jugador
                        :nombre   '|Ju-Erroneo|
                        :f-juego  #'f-j-nmx
                        :f-eval   #'f-eval-erronea))

;;; Jugador Bueno (debido a que juega con un nivel mas de evaluacion)
;;; ------------------------------------------------------------------------------------------
(defun f-eval-Bueno (estado)
  (if (juego-terminado-p estado)
      -50                              ;; Condicion especial de juego terminado
    ;; Devuelve el maximo del numero de fichas del lado enemigo menos el numero de propias
    (max-list (mapcar #'(lambda(x)
                          (- (suma-fila (estado-tablero x) (lado-contrario (estado-lado-sgte-jugador x)))
                             (suma-fila (estado-tablero x) (estado-lado-sgte-jugador x))))
                (generar-sucesores estado)))))

(defvar *jdr-nmx-Bueno* (make-jugador
                        :nombre   '|Ju-Nmx-Bueno|
                        :f-juego  #'f-j-nmx
                        :f-eval   #'f-eval-Bueno))

;;; Devuelve el top segun test de una lista de nos. y su posicion
(defun max-list (l &key (test #'max))
  (let ((m (reduce test l)))
    (values m (position m l))))

;;; (max-list '(2 5 3 6 2 0))  -> 6, 3
;;; (max-list '(2 5 3 6 2 0) :test #'Min)  -> 0, 5

;;; Jugador Regular
;;; ------------------------------------------------------------------------------------------
(defun f-eval-Regular (estado)
  (- (suma-fila (estado-tablero estado) (estado-lado-sgte-jugador estado))
     (suma-fila (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)))))

(defvar *jdr-nmx-Regular* (make-jugador
                        :nombre   '|Ju-Nmx-Regular|
                        :f-juego  #'f-j-nmx
                        :f-eval   #'f-eval-Regular))

;;; ------------------------------------------------------------------------------------------
;;; EJEMPLOS DE PARTIDAS DE PRUEBA
;;; ------------------------------------------------------------------------------------------
;;; Juego manual contra jugador automatico, saca el humano
;(partida 0 2 (list *jdr-humano*      *jdr-nmx-Bueno* ))

;;; Juego manual contra jugador automatico, saca el automatico
;(partida 1 2 (list *jdr-humano*      *jdr-nmx-Bueno* ))

;;; Juego automatico sin presentacion del tablero pero con listado de contador
;(setq *verjugada* nil)   ; valor por defecto
;(setq *vermarcador* t)   ; valor por defecto
;(partida 0 1 (list *jdr-aleatorio*   *jdr-nmx-Bueno*))

;;; Juego automatico con salida mimima por eficiencia, profundidad=2
;;;(setq *verb* nil *debug-level* 0 *verjugada* nil *vermarcador* nil)
;;; Ajustes para facilitar el seguimiento paso a paso (pag. 11). Reduzcase el nivel de
;;; detalle cuando se vaya adquiriendo práctica.
;;; *debug-nmx* activa *verb* tambien para jugadores automaticos (normalmente desactivado).
(setq *debug-level* 2)         ; Ajusta a 2 el nivel de detalle
(setq *verb*        nil)         ; Activa comentarios para seguir la evolucion de la partida
(setq *verjugada*   t)         ; Activa la visualizacion de jugadas
(setq *vermarcador* t)         ; Activa la visualizacion del marcador
(setq *debug-nmx*   t)         ; Desactiva debuging de negamax

(partida 1 2 (list *jdr-nmx-Regular* *jdr-nmx-Regular*))
(partida 1 2 (list *jdr-aleatorio* *jdr-aleatorio*))

;;; Ajustes para facilitar el seguimiento paso a paso (pag. 11). Reduzcase el nivel de
;;; detalle cuando se vaya adquiriendo práctica.
;;; *debug-nmx* activa *verb* tambien para jugadores automaticos (normalmente desactivado).
(setq *debug-level* 2)         ; Ajusta a 2 el nivel de detalle
(setq *verb* t)                ; Activa comentarios para seguir la evolucion de la partida
(setq *verjugada*   t)         ; Activa la visualizacion de jugadas
(setq *vermarcador* t)         ; Activa la visualizacion del marcador
(setq *debug-nmx* nil)         ; Desactiva debuging de negamax
;(partida 0 2 (list *jdr-humano*  *jdr-nmx-Regular*))

;;; Partida entre dos humanos a partir de una posicion determinada para analisis detallado
;;; Se fuerza la posicion de inicio para jugar a partir de ella (ejemplo Pag.5 del enunciado)
;;;(setq mi-posicion (list '(1 0 1 3 2 4 0) (reverse '(12 0 3 5 2 1 2))))
;;;(setq mi-posicion (list '(1 0 1 3 2 4 7) (reverse '(5 0 3 5 2 1 2))))
;;;(setq mi-posicion (list '(0 3 1 1 1 8 8) (reverse '(4 0 8 2 0 0 0))))
;;;(setq estado (crea-estado-inicial 0 mi-posicion))
;;;(partida 0 2 (list *jdr-humano*      *jdr-human2*) mi-posicion)

;;; Fuerza posicion: fin juego inevitable a la siguiente jugada
;(setq mi-posicion (list '(2 1 3 0 2 7 29) (reverse '(2 5 0 0 0 0 0))))

;;; Ejemplo de experimentacion a varias profundidades
;;;(setq *debug-level* 2)
;;;(setq *verjugada*   nil)
;;;(setq *vermarcador* nil)
;;;(dolist (n '(1 2 3 4 5)) (print (partida 2 n (list *jdr-nmx-regular* *jdr-nmx-bueno*))))

;;; Timeout jugada: a nivel 8 el aleatorio pierde por tiempo
;;;(partida 0 1 (list *jdr-humano*      *jdr-nmx-eval-aleatoria*))
;;;(partida 0 8 (list *jdr-humano*      *jdr-nmx-eval-aleatoria*))

;;; Ejemplos de partidas para pruebas
;;;(partida 0 2 (list *jdr-nmx-Regular* *jdr-erroneo*))
;;;(partida 0 2 (list *jdr-nmx-Regular* *jdr-nmx-bueno*))
;;;(partida 0 2 (list *jdr-humano*      *jdr-nmx-Regular*))
;;;(partida 0 2 (list *jdr-humano*      *jdr-nmx-Bueno*))
;;;(partida 0 2 (list *jdr-humano*      *jdr-1st-opt*))
;;;(partida 0 2 (list *jdr-humano*      *jdr-last-opt*))
;;;(partida 0 2 (list *jdr-humano*      *jdr-human2*))
