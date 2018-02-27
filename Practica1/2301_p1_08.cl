
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;           PRACTICA 1 - INTELIGENCIA ARTIFICIAL         ;;;
;;; Andres Salas Peña                                      ;;;
;;; Ricardo Riol Gonzalez                                  ;;;
;;; Grupo 2301                                   Pareja 09 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    EJERCICIO 1                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 1.1.1
;;; sc-rec (x y)
;;; Calcula la similitud coseno de un vector de forma recursiva
;;;
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;; La semejanza coseno entre dos vectores que son listas vacías o que son
;;; (0 0 ... 0) es NIL
;;;
;;; INPUT: x: vector, representado como una lista
;;; y: vector, representado como una lista
;;;
;;; OUTPUT: similitud coseno entre x e y
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Funcion que comprueba si las listas cumplen:
;; 1) Son distintas de (000..0)
;; 2) Son distintas de nil
;; Devuelve nil en caso de que no se cumpla 1) y 2) t en caso contrario

(defun is-ok (x y)
  (cond ((or (equal nil x) (equal nil y)) nil)
        ((or (every #' zerop x) (every #' zerop y)) nil)
        (t t)))

(is-ok '(1 2 3) '(3 4 5)) ;; t
(is-ok '(1 2 3) '(3 4 -5)) ;; nil
(is-ok '(0 0 0) '(1 0 0));; nil
(is-ok '(1 0 0) '());; nil


(defun our-pesc-rec (x y)                    ;;Calcula el producto escalar de dos vectores representados como listas
  (if (or (equal nil x) (equal nil y))       ;; Si x o y es null devolvemos 0
      0
    (+ (* (first x) (first y))
       (our-pesc-rec (rest x) (rest y)))))  ;; Calculamos sum ( x(i) * y(i)) con 1 < i < long x

(our-pesc-rec '(1 2 3) '( 2 -5 6))
  
(defun sc-rec (lista1 lista2)
  (if (equal NIL (is-ok lista1 lista2))     ;;Comprobamos que la lista cumple las condiciones del enunciado
      NIL
    (/ (our-pesc-rec lista1 lista2)
       (*(sqrt (our-pesc-rec lista1 lista1)) (sqrt (our-pesc-rec lista2 lista2))))))

;; El producto escalar de un vector consigo mismo es su norma al cuadrado

(sc-rec '(1 0) '(0 1)) ;; 0.0
(sc-rec '() '(0 1)) ;; nil
(sc-rec '(1 2 3) '(1 0 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 1.1.2
;;; sc-mapcar (x y)
;;; Calcula la similitud coseno de un vector usando mapcar
;;;
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;; La semejanza coseno entre dos vectores que son listas vacías o que son
;;; (0 0 ... 0) es NIL
;;;
;;; INPUT: x: vector, representado como una lista
;;; y: vector, representado como una lista
;;;
;;; OUTPUT: similitud coseno entre x e y
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun our-pesc-map (lista1 lista2)
  (reduce '+ (mapcar #'* lista1 lista2)))     ;; Sum (x(i) * y(i)) com 1< i <len x

(defun sc-mapcar (lista1 lista2)              ;; Utilizamos las funciones recursivas del primer apartado.
  (if (equal NIL (is-ok lista1 lista2))
      nil
     (/ (our-pesc-map lista1 lista2) (*(sqrt (our-pesc-map lista1 lista1)) (sqrt (our-pesc-map lista2 lista2))))))

(sc-mapcar '(1 2 3) '(1 0 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 1.2
;;;	sc-conf (cat vs conf)
;;; Devuelve aquellos vectores similares a una categoria
;;;
;;; INPUT: cat: vector, representado como una lista
;;; vs: vector de vectores, representado como una lista de listas
;;; conf: Nivel de confianza
;;;
;;; OUTPUT: Vectores cuya similitud es superior al nivel de confianza, ordenados
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun our-conf (cat vs conf)
  (remove-if #'(lambda (y) (< (sc-mapcar cat y) conf)) vs))     ;; Eliminamos de vs las listas cuyo cos con x sea menor que una constante dada

(our-conf '(1 2 3) '((1 2 3) (2 3 4) (1 0 0)) 0.5)
(sc-mapcar '(1 2 3) '(1 0 0))

(defun sc-conf (cat vs conf)
  (sort (our-conf cat vs conf)
        #'(lambda (z y) (> (sc-mapcar cat z) (sc-mapcar cat y))))) ;; Ordenamos de mayor a menor el vector en funcion de su cos con la lista x.

(sc-conf '(1 2 3) '((1 2 3) (3 4 5) (1 0 0) (1 1 1)) 0.9)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 1.3
;;; sc-classifier (cats texts func)
;;; Clasifica a los textos en categorias.
;;;
;;; INPUT: cats: vector de vectores, representado como una lista de listas
;;; texts: vector de vectores, representado como una lista de listas
;;; func: referencia a funcion para evaluar la similitud coseno
;;;
;;; OUTPUT: Pares identificador de categori­a con resultado de similitud coseno
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun our-similarity-cos (cats lista func)                ;; Forma una lista de pares de la forma (vector , cos <lista y>) con y perteneciente a cats
  (mapcar #'(lambda (y) (append(list(first y) (funcall func (rest lista) (rest y)))))
    cats))
  
(our-similarity-cos '((1 2 3) ( 2 3 4) (6 6 8)) '(3 3 3) #'sc-rec)

(defun our-max-similarity (cats lista func)                ;; Ordena los pares en funcion de la segunda coordenada.
  (first (sort(our-similarity-cos cats lista func)
               #'(lambda (z y) (> (second z) (second y))))))

(our-max-similarity '((1 2 3) ( 2 3 5) (6 6 8)) '( 3 6 8) #'sc-rec)

(defun sc-classifier (cats texts func)                     ;;Aplica las funciones anteriores a un conjunto de vectores.
  (mapcar #'(lambda (z) (our-max-similarity cats z func)) texts))


(sc-classifier '((1 2 3) (2 3 5) (3 6 8)) '((1 3 5) (2 6 8)) #'sc-rec)
(sc-classifier '((1 2 3) (2 3 5) (3 6 8)) '((1 3 5) (2 3 6) (3 2 3)) #'sc-rec)
(sc-classifier '((1 2 3) (2 3 5) (3 6 8)) '((1 3 5) (2 6 8)) #'sc-mapcar)
(sc-classifier '((1 2 3) (2 3 5) (3 6 8)) '((1 3 5) (2 3 6) (3 2 3)) #'sc-mapcar)
(sc-classifier '((1 43 23 12) (2 33 54 24)) '((1 3 22 134) (2 43 26 58)) #'sc-rec) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Estudio de tiempos
(time (sc-classifier '((1 2 3 4 5 6 7 3 4 4 4 4 4 4 5) (2 3 5 5 6 7 8 2 2 2 2 2 3 4 5) (3 6 8 6 7 7 7 1 2 1 2 3 5 6 7)) '((1 3 5 2 2 2 2 6 8 4 2 1 9 9 9) (2 6 8 4 3 4 6 1 3 4 6 7 2 2 2)) #'sc-rec))
;;real time  0.002000 sec
(time (sc-classifier '((1 2 3 4 5 6 7 3 4 4 4 4 4 4 5) (2 3 5 5 6 7 8 2 2 2 2 2 3 4 5) (3 6 8 6 7 7 7 1 2 1 2 3 5 6 7)) '((1 3 5 2 2 2 2 6 8 4 2 1 9 9 9) (2 6 8 4 3 4 6 1 3 4 6 7 2 2 2)) #'sc-mapcar))
;;real time  0.000000 sec
(time (sc-classifier (make-list 8 :initial-element (make-list 20 :initial-element 1)) (make-list 8 :initial-element (make-list 20 :initial-element 2)) #'sc-rec))
;;real time  0.035000 sec
(time (sc-classifier (make-list 8 :initial-element (make-list 20 :initial-element 1)) (make-list 8 :initial-element (make-list 20 :initial-element 2)) #'sc-mapcar))
;;real time  0.006000 sec
(time (sc-classifier (make-list 12 :initial-element (make-list 50 :initial-element 1)) (make-list 12 :initial-element (make-list 50 :initial-element 2)) #'sc-rec))
;;real time  0.182000 sec
(time (sc-classifier (make-list 12 :initial-element (make-list 50 :initial-element 1)) (make-list 12 :initial-element (make-list 50 :initial-element 2)) #'sc-mapcar))
; real time  0.014000 sec
(time (sc-classifier (make-list 15 :initial-element (make-list 100 :initial-element 1)) (make-list 15 :initial-element (make-list 100 :initial-element 2)) #'sc-rec))
;;real time  0.568000 sec
(time (sc-classifier (make-list 15 :initial-element (make-list 100 :initial-element 1)) (make-list 15 :initial-element (make-list 100 :initial-element 2)) #'sc-mapcar))
;; real time  0.025000 sec 

;; Los resultados obtenidos se comentarán en la memoria


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    EJERCICIO 2                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 2.1
;;; bisect (f a b tol)
;;; Encuentra una raiz de f entre los puntos a y b usando biseccion
;;;
;;; Si f(a)f(b)>=0 no hay garanti­a de que vaya a haber una rai­z en el
;;; intervalo, y la funcion devolvera NIL.
;;;
;;; INPUT: f: funcion de un solo parametro real con valores reales cuya
;;; raiz queremos encontrar
;;; a: extremo inferior del intervalo en el que queremos buscar la raiz
;;; b: b>a extremo superior del intervalo en el que queremos buscar la rai­z
;;; tol: tolerancia para el criterio de parada: si b-a < tol de la funcion
;;;
;;; OUTPUT: raiz de la funcion, o NIL si no se encuentra raiz
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun our-distance (a b)                            ;;Calcula la distancia entre dos reales
  (abs (- b a)))

(defun our-medium-point (a b)                        ;;Calcula el punto medio entre dos extremos reales
  (/ (+ a b) 2))

 (defun bisect (f a b tol)                           ;; Aplica el algoritmo de la biseccion para encontrar soluciones de f (f(a) = 0).
  (let ((c (our-medium-point a b)))                  ;; Definimos c = punto medio de (a , b)
    (cond ((equal (funcall f a) 0) a)                ;; Si f(a) = 0 -> a
        ((equal (funcall f b) 0) b)                  ;; Si f(b) = 0 -> b
        ((> (* (funcall f a) (funcall f b)) 0) nil)  ;; Si f(a) * f(b) > 0 -> nil (ambas son positvas o negativas)
        ((< (our-distance a b) tol) c)               ;; Si el tamanyo de (a , b) es menor que tol -> punto medio (a , b)
        ((= (funcall f c) 0) c) ;; Si f(c) = 0 -> c
        ((< (* (funcall f a) (funcall f c)) 0) (bisect f a c tol))    ;; Aplicamos la recursividad en uno de los dos intervalos 
        ((< (* (funcall f b) (funcall f c)) 0) (bisect f c b tol)))))
         
(bisect #'(lambda (z) (* z z z)) -4 5 0.5)
(bisect #'(lambda(x) (sin (* 6.26 x))) 0.0 0.7 0.001)
(bisect #'(lambda(x) (sin (* 6.28 x))) 1.1 1.5 0.001)
(bisect #'(lambda(x) (sin (* 6.28 x))) 1.1 2.1 0.001)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 2.2
;;; allroot (f lst tol)
;;; Encuentra todas las raices localizadas entre dos valores consecutivos
;;; de una lista de valores
;;;
;;; INPUT: f: funcion de un solo parametro real con valores reales cuya
;;; rai­z queremos encontrar
;;; lst: lista ordenada de valores reales (lst[i] < lst[i+1])
;;; tol: tolerancia para el criterio de parada: si b-a < tol de la funcion
;;;
;;; OUTPUT: una lista o valores reales que contienen las raices de la funcion
;;; en los subintervalos dados.
;;;
;;; Cuando sgn(f(lst[i])) != sgn(f(lst[i+1])) esta funcion busca
;;; una raiz en el correspondiente intervalo 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun allroot (f lst tol)
  (if (or (equal lst nil) (equal (rest lst) nil))            ;; Caso base: Si la lista no tiene elementos o solo 1.
      nil
    (mapcan #'(lambda (x) (unless (null x) (list x)))        ;;Lo utilizamos para eliminar los nil de la lista
    (append (list (bisect f (first lst) (second lst) tol))   ;; lista de soluciones
            (allroot f (rest lst) tol))))) 

(allroot #'(lambda(x) (sin (* 6.28 x))) '(0.25 0.75 1.25 1.75 2.25) 0.0001)
(allroot #'(lambda(x) (sin (* 6.28 x))) '(0.25 0.9 0.75 1.25 1.75 2.25) 0.0001) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 2.3
;;; allind (f a b N tol)
;;;	Divide en un numero 2^N de invervalos y encuentra todas las raices
;;; de la funcion f en los intervalos obtenidos
;;;
;;; INPUT: f: funcion de un solo parametro real con valores reales cuya
;;; raiz queremos encontrar
;;; a: extremo inferior del intervalo en el que buscamos la rai­z
;;; b: b>a extremo superior del intervalo en el que queremos buscar la rai­z
;;; N: exponente del numero de intervalos en el que [a, b] va a ser dividido
;;; [a, b] es dividido en 2^N intervalos
;;; tol: tolerancia para el critreio de parada: si b-a < tol de la funcion
;;;
;;; OUTPUT: lista con todas las raices encontradas
;;;
;;; El intervalo (a, b) es dividido en intervalos (x[i], x[i+1]) con
;;; x[i] = a + i*dlt; una raiz es buscada en cada intervalo, y todas las
;;; rai­ces encontradas se juntan en una lista que se devuelve
;;;
;;; Pista:
;;; Uno puede encontrar una manera de usar allroot para implementar esta funcion.
;;; Esto es posible por supuesto, pero hay una forma simple de hacerlo recursivo
;;; sin usar allroot.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun div-int (a b N)                                           ;; Dividimos un intervalo (a , b) en 2^n intervalos
  (let ((c (our-medium-point a b)))                              ;; Definimos c = punto intermedio
    (if (equal N 0) ;;Caso base: N = 0 -> 1 intervalo
        (list a)
      (append (div-int a c (- N 1))
              (div-int c b (- N 1))))))                           ;; Dividimos cada intervalo por la mitad hasta que n = 0

;; La funcion anterior nos devuelve una lista con todos las divisiones del intervalo sin incluir el extremo b por eso en la siguiente funcion se anyade a mano.


(defun allind (f a b N tol) 
  (allroot f (append (div-int a b N) (list b)) tol))             ;;Anyadimos b y calculamos todas las soluciones con la funcion del apartado 2.2



(allind #'(lambda(x) (sin (* 6.28 x))) 0 3 2 0.01) ;
(allind #'(lambda(x) (sin (* 6.28 x))) 0.1 2.25 3 0.0001)
(allind #'(lambda(x) (sin (* 6.28 x))) 0.25 2.25 3 0.01) ;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    EJERCICIO 3                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 3.1
;;; combine-elt-lst (elem lst)
;;; Combina un elemento dado con todos los elementos de una lista
;;;
;;; INPUT: elt: elemento que se combinara con los de la lista
;;; lst: lista con la que se combinara el elemento
;;;
;;; OUTPUT: devuelve la lista con las combinaciones del elemento y la lista dadas.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun combine-elem-lst (elem lst) 
  (let ((k (list(list elem (first lst)))))                             ;; Con esta definicion no repetimos codigo
  (cond ((equal lst nil) nil)                                          ;; Caso base: lst nil -> nil
        ((equal (rest lst) nil) k)                                     ;; Caso base: si la lista tiene un elemento -> k
        ( t (append k (combine-elem-lst elem (rest lst)))))))

(combine-elem-lst 'a nil)
(combine-elem-lst 'a '(1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 3.2
;;; combine-lst-lst (lst1 lst2)
;;; Calcula el producto cartesiano de dos listas
;;;
;;; INPUT: lst1: primera lista sobre la que se realizara el producto cartesiano
;;; lst2: segunda lista sobre la que se realizara el producto cartesiano
;;;
;;; OUTPUT: devuelve la lista resultado del producto cartesiando de las anteriores
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun combine-lst-lst (lst1 lst2)
  (if (or (equal lst1 nil) (equal lst2 nil))                            ;; Caso base: Si ambas lista son nil -> nil
      nil
    (append (combine-elem-lst (first lst1) lst2) 
            (combine-lst-lst (rest lst1) lst2))))                       ;; Combinamos el elemento primero de la primera lista con la segunda
                                                                        ;; y aplicamos recursividad sobre la lst1

(combine-lst-lst nil nil)
(combine-lst-lst '(a b c) nil)
(combine-lst-lst NIL '(a b c))
(combine-lst-lst '(a b c) '(1 2))
(combine-lst-lst '(a b c d e f) '(1 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 3.3
;;; combine-list-of-lsts (lstolsts)
;;; Calcula todas las posibles disposiciones pertenecientes a N listas 
;;; de forma que en cada disposicion aparezca solo un elemento de cada lista
;;;
;;; INPUT: lstolsts: todas las listas que se combinaran
;;;
;;; OUTPUT: devuelve una lista resultado de la combinacion de todas las dadas
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flatten (lst)  ;; Funcion que sirve para quitar parentesis a las listas
  (cond 
   ((null lst) NIL) 
   ((atom (first lst)) 
    (cons (first lst) (flatten (rest lst)))) 
   (t (append (flatten (first lst)) (flatten (rest lst))))))

(defun cl (lstolsts)
  (if (equal lstolsts nil)                                             ;; Si lstolsts es nil devolvemos (nil)
      (return-from cl nil))
  (let ((comb (cl (rest lstolsts))))                                   ;;Definimos (cl (rest lstoslts)) = comb
    (if (eql comb nil)                                                 ;;Si ha finalizado de recorrer la lista de listas devolvemos todo lo anterior
       (first lstolsts)
      (combine-lst-lst (first lstolsts) comb))))                       ;; LLamada recursiva

(defun combine-list-of-lsts (lstolsts)
  (cond ((equal lstolsts nil) '(nil))                                  ;; Si el vector de listas es nil -> nil
        ((some #'(lambda (z) (equal z nil)) lstolsts) nil)             ;; Si alguna lista de vector es nil -> nil
        ((equal (rest lstolsts) nil) (mapcar #'list (first lstolsts))) ;; Si el vector tiene solo una lista -> Convertimos cada elemento en una lista independiente
        ( t (mapcar #'(lambda (x) (flatten x)) (cl lstolsts)))))       ;; En caso contrario, llamamos a la funcion recursciva cl

(combine-list-of-lsts '((a b c) (+ -) (1 2 3 4) (m o r e)))
(combine-list-of-lsts '(() (+ -) (1 2 3 4)))
(combine-list-of-lsts '((a b c) () (1 2 3 4)))
(combine-list-of-lsts '((a b c) (1 2 3 4) ()))
(combine-list-of-lsts '((1 2 3 4)))
(combine-list-of-lsts '((a b c) (+ -) (1 2 3 4)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    EJERCICIO 4                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definicion de simbolos que representan valores de verdad,
;; conectores y predicados para evaluar si una expresion LISP
;; es un valor de verdad o un conector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +bicond+ '<=>)
(defconstant +cond+   '=>)
(defconstant +and+    '^)
(defconstant +or+     'v)
(defconstant +not+    '¬)

(defun truth-value-p (x) 
  (or (eql x T) (eql x NIL)))

(defun unary-connector-p (x) 
  (eql x +not+))

(defun binary-connector-p (x) 
  (or (eql x +bicond+) 
      (eql x +cond+)))

(defun n-ary-connector-p (x) 
  (or (eql x +and+) 
      (eql x +or+)))

(defun connector-p (x) 
  (or (unary-connector-p  x)
      (binary-connector-p x)
      (n-ary-connector-p   x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.1
;; Predicado para determinar si una expresion en LISP
;; es un literal positivo 
;;
;; RECIBE   : expresion 
;; EVALUA A : T si la expresion es un literal positivo, 
;;            NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun positive-literal-p (x)
  (and (atom x)
       (not (truth-value-p x))
       (not (connector-p x))))

;; EJEMPLOS:
(positive-literal-p 'p)
;; evalua a T
(positive-literal-p T)
(positive-literal-p NIL)
(positive-literal-p '¬)
(positive-literal-p '=>)
(positive-literal-p '(p))
(positive-literal-p '(¬ p))
(positive-literal-p '(¬ (v p q)))
;; evaluan a NIL


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.2
;; Predicado para determinar si una expresion
;; es un literal negativo 
;;
;; RECIBE   : expresion x 
;; EVALUA A : T si la expresion es un literal negativo, 
;;            NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun negative-literal-p (x)
  (and
   (listp x)
   (null  (cddr x))
   (unary-connector-p (first x))
   (positive-literal-p (first (rest x)))))

;; EJEMPLOS:
(negative-literal-p '(¬ p))        ; T
(negative-literal-p NIL)           ; NIL
(negative-literal-p '¬)            ; NIL
(negative-literal-p '=>)           ; NIL
(negative-literal-p '(p))          ; NIL
(negative-literal-p '((¬ p)))      ; NIL
(negative-literal-p '(¬ T))        ; NIL
(negative-literal-p '(¬ NIL))      ; NIL
(negative-literal-p '(¬ =>))       ; NIL
(negative-literal-p 'p)            ; NIL
(negative-literal-p '((¬ p)))      ; NIL
(negative-literal-p '(¬ (v p q)))  ; NIL


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.3
;; Predicado para determinar si una expresion es un literal  
;;
;; RECIBE   : expresion x  
;; EVALUA A : T si la expresion es un literal, 
;;            NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun literal-p (x) 
  (or (positive-literal-p x) 
      (negative-literal-p x)))

;; EJEMPLOS:
(literal-p 'p)             
(literal-p '(¬ p))      
;;; evaluan a T
(literal-p '(p))
(literal-p '(¬ (v p q)))
;;; evaluan a  NIL


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicado para determinar si una expresion esta en formato prefijo 
;;
;; RECIBE   : expresion x 
;; EVALUA A : T si x esta en formato prefijo, NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun wff-prefix-p (x)
  (unless (null x)             ;; NIL no es FBF en formato prefijo (por convencion)
    (or (literal-p x)          ;; Un literal es FBF en formato prefijo
        (and (listp x)         ;; En caso de que no sea un literal debe ser una lista
             (let ((connector (first x))
                   (rest_1    (rest  x)))
               (cond
                ((unary-connector-p connector)  ;; Si el primer elemento es un connector unario
                 (and (null (rest rest_1))      ;; deberia tener la estructura (<conector> FBF)
                      (wff-prefix-p (first rest_1)))) 
                ((binary-connector-p connector) ;; Si el primer elemento es un conector binario
                 (let ((rest_2 (rest rest_1)))  ;; deberia tener la estructura 
                   (and (null (rest rest_2))    ;; (<conector> FBF1 FBF2)
                        (wff-prefix-p (first rest_1))
                        (wff-prefix-p (first rest_2)))))               
                ((n-ary-connector-p connector)  ;; Si el primer elemento es un conector enario
                 (or (null rest_1)              ;; conjuncion o disyuncion vacias
                     (and (wff-prefix-p (first rest_1)) ;; tienen que ser FBF los operandos 
                          (let ((rest_2 (rest rest_1)))
                            (or (null rest_2)           ;; conjuncion o disyuncion con un elemento
                                (wff-prefix-p (cons connector rest_2)))))))	
                (t NIL)))))))                   ;; No es FBF en formato prefijo 
;;
;; EJEMPLOS:
(wff-prefix-p '(v))
(wff-prefix-p '(^))
(wff-prefix-p '(v A))
(wff-prefix-p '(^ (¬ B)))
(wff-prefix-p '(v A (¬ B)))
(wff-prefix-p '(v (¬ B) A ))
(wff-prefix-p '(^ (V P (=> A (^ B (¬ C) D))) (^ (<=> P (¬ Q)) P) E))
;;; evaluan a T
(wff-prefix-p 'NIL)
(wff-prefix-p '(¬))
(wff-prefix-p '(=>))
(wff-prefix-p '(<=>))
(wff-prefix-p '(^ (V P (=> A ( B ^ (¬ C) ^ D))) (^ (<=> P (¬ Q)) P) E))
;;; evaluan a NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.4
;; Predicado para determinar si una expresion esta en formato prefijo 
;;
;; RECIBE   : expresion x 
;; EVALUA A : T si x esta en formato prefijo, 
;;            NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun wff-infix-p (x)
  (unless (null x)
    (or (literal-p x)
        (and (listp x)
             (let ((op1 (car x))
                   (exp1 (cadr x))
                   (list_exp2 (cddr x)))
               (cond 
                ((unary-connector-p op1)
                 (and (null list_exp2)
                      (wff-infix-p exp1)))
                ((n-ary-connector-p op1)
                 (null (rest x)))
                ((binary-connector-p exp1)
                 (and (wff-infix-p op1)
                      (null (cdr list_exp2))
                      (wff-infix-p (car list_exp2))))
                ((n-ary-connector-p exp1)
                 (and (wff-infix-p op1)
                      (nop-verify exp1 (cdr x))))
                 (t NIL)))))))

;; Verifica si una expresion de la forma op <wff> ... op <wff>
(defun nop-verify (op exp)
  (or (null exp)
      (and (equal op (car exp))
           (wff-infix-p (cadr exp))
           (nop-verify op (cddr exp)))))

;;
;; EJEMPLOS:
;;
(wff-infix-p 'A) 						; T
(wff-infix-p '(^)) 					; T  ;; por convencion
(wff-infix-p '(v)) 					; T  ;; por convencion
(wff-infix-p '(A ^ (v))) 			      ; T  
(wff-infix-p '( a ^ b ^ (p v q) ^ (¬ r) ^ s))  	; T 
(wff-infix-p '(A => B)) 				; T
(wff-infix-p '(A => (B <=> C))) 			; T
(wff-infix-p '( B => (A ^ C ^ D))) 			; T   
(wff-infix-p '( B => (A ^ C))) 			; T 
(wff-infix-p '( B ^ (A ^ C))) 			; T 
(wff-infix-p '((p v (a => (b ^ (¬ c) ^ d))) ^ ((p <=> (¬ q)) ^ p ) ^ e))  ; T 
(wff-infix-p nil) 					; NIL
(wff-infix-p '(a ^)) 					; NIL
(wff-infix-p '(^ a)) 					; NIL
(wff-infix-p '(a)) 					; NIL
(wff-infix-p '((a))) 				      ; NIL
(wff-infix-p '((a) b))   			      ; NIL
(wff-infix-p '(^ a b q (¬ r) s))  		      ; NIL 
(wff-infix-p '( B => A C)) 			      ; NIL   
(wff-infix-p '( => A)) 				      ; NIL   
(wff-infix-p '(A =>)) 				      ; NIL   
(wff-infix-p '(A => B <=> C)) 		      ; NIL
(wff-infix-p '( B => (A ^ C v D))) 		      ; NIL   
(wff-infix-p '( B ^ C v D )) 			      ; NIL 
(wff-infix-p '((p v (a => e (b ^ (¬ c) ^ d))) ^ ((p <=> (¬ q)) ^ p ) ^ e)); NIL 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convierte FBF en formato prefijo a FBF en formato infijo
;;
;; RECIBE   : FBF en formato prefijo 
;; EVALUA A : FBF en formato infijo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prefix-to-infix (wff)
  (when (wff-prefix-p wff)
    (if (literal-p wff)
        wff
      (let ((connector (first wff))
            (elements-wff (rest wff)))
        (cond
         ((unary-connector-p connector) 
          (list connector (prefix-to-infix (second wff))))
         ((binary-connector-p connector) 
          (list (prefix-to-infix (second wff))
                connector
                (prefix-to-infix (third wff))))
         ((n-ary-connector-p connector) 
          (cond 
           ((null elements-wff)        ;;; conjuncion o disyuncion vacias. 
            wff)                       ;;; por convencion, se acepta como fbf en formato infijo
           ((null (cdr elements-wff))  ;;; conjuncion o disyuncion con un unico elemento
            (prefix-to-infix (car elements-wff)))  
           (t (cons (prefix-to-infix (first elements-wff)) 
                    (mapcan #'(lambda(x) (list connector (prefix-to-infix x))) 
                      (rest elements-wff))))))
         (t NIL)))))) ;; no deberia llegar a este paso nunca

;;
;;  EJEMPLOS:
;;
(prefix-to-infix nil) 
(prefix-to-infix '(v))          ; (V)
(prefix-to-infix '(^))          ; (^)
(prefix-to-infix '(v a))        ; A
(prefix-to-infix '(^ a))        ; A
(prefix-to-infix '(^ (¬ a)))    ; (¬ a)
(prefix-to-infix '(v a b))      ; (A v B)
(prefix-to-infix '(v a b c))    ; (A V B V C)
(prefix-to-infix '(^ (V P (=> A (^ B (¬ C) D))) (^ (<=> P (¬ Q)) P) E))
;;; ((P V (A => (B ^ (¬ C) ^ D))) ^ ((P <=> (¬ Q)) ^ P) ^ E)
(prefix-to-infix '(^ (v p (=> a (^ b (¬ c) d))))) ; (P V (A => (B ^ (¬ C) ^ D)))
(prefix-to-infix '(^ (^ (<=> p (¬ q)) p ) e))     ; (((P <=> (¬ Q)) ^ P) ^ E)  
(prefix-to-infix '( v (¬ p) q (¬ r) (¬ s)))       ; ((¬ P) V Q V (¬ R) V (¬ S))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.5
;;
;; Convierte FBF en formato infijo a FBF en formato prefijo
;;  
;; RECIBE   : FBF en formato infijo 
;; EVALUA A : FBF en formato prefijo 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun infix-to-prefix (wff)
  (when (wff-infix-p wff)
    (if (literal-p wff)
        wff
      (let ((op1 (first wff))
            (rst_ele (rest wff))
            (conector (first (rest wff))))
        (cond
         ((unary-connector-p op1)
          (list op1 (infix-to-prefix (second wff))))
         ((binary-connector-p conector)
          (list conector (infix-to-prefix op1) (infix-to-prefix (third wff))))
         ((n-ary-connector-p conector)
          (cond
           ((null rst_ele) wff)
           ((null (cdr rst_ele))
            (infix-to-prefix op1))
           (t (cons conector (mapcar #'(lambda (x) (infix-to-prefix x)) (list-def wff conector) )))))
         (t nil))))))
          
(defun list-def (wff conector)
  (remove-if #'(lambda (x) (equal conector x)) wff))
  
          
(infix-to-prefix '(( a => b) v b v c))           
           

            
;;
;; EJEMPLOS
;;
(literal-p 'a)
(infix-to-prefix nil)      ;; NIL
(infix-to-prefix 'a)       ;; a
(infix-to-prefix '((a)))   ;; NIL
(infix-to-prefix '(a))     ;; NIL
(infix-to-prefix '(((a)))) ;; NIL
(prefix-to-infix (infix-to-prefix '((p v (a => (b ^ (¬ c) ^ d))) ^ ((p <=> (¬ q)) ^ p) ^ e)) ) 
;;-> ((P V (A => (B ^ (¬ C) ^ D))) ^ ((P <=> (¬ Q)) ^ P) ^ E)


(infix-to-prefix '((p v (a => (b ^ (¬ c) ^ d))) ^  ((p <=> (¬ q)) ^ p) ^ e))  
;; (^ (V P (=> A (^ B (¬ C) D))) (^ (<=> P (¬ Q)) P) E)

(infix-to-prefix '(¬ ((¬ p) v q v (¬ r) v (¬ s))))
;; (¬ (V (¬ P) Q (¬ R) (¬ S)))


(infix-to-prefix
 (prefix-to-infix
  '(V (¬ P) Q (¬ R) (¬ S))))
;;-> (V (¬ P) Q (¬ R) (¬ S))

(infix-to-prefix
 (prefix-to-infix
  '(¬ (V (¬ P) Q (¬ R) (¬ S)))))
;;-> (¬ (V (¬ P) Q (¬ R) (¬ S)))


(infix-to-prefix 'a)  ; A
(infix-to-prefix '((p v (a => (b ^ (¬ c) ^ d))) ^  ((p <=> (¬ q)) ^ p) ^ e))  
;; (^ (V P (=> A (^ B (¬ C) D))) (^ (<=> P (¬ Q)) P) E)

(infix-to-prefix '(¬ ((¬ p) v q v (¬ r) v (¬ s))))
;; (¬ (V (¬ P) Q (¬ R) (¬ S)))

(infix-to-prefix  (prefix-to-infix '(^ (v p (=> a (^ b (¬ c) d)))))) ; '(v p (=> a (^ b (¬ c) d))))
(infix-to-prefix  (prefix-to-infix '(^ (^ (<=> p (¬ q)) p ) e))) ; '(^ (^ (<=> p (¬ q)) p ) e))  
(infix-to-prefix (prefix-to-infix '( v (¬ p) q (¬ r) (¬ s))))  ; '( v (¬ p) q (¬ r) (¬ s)))
;;;

(infix-to-prefix '(p v (a => (b ^ (¬ c) ^ d)))) ; (V P (=> A (^ B (¬ C) D)))
(infix-to-prefix '(((P <=> (¬ Q)) ^ P) ^ E))  ; (^ (^ (<=> P (¬ Q)) P) E)
(infix-to-prefix '((¬ P) V Q V (¬ R) V (¬ S))); (V (¬ P) Q (¬ R) (¬ S))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.6
;; Predicado para determinar si una FBF es una clausula  
;;
;; RECIBE   : FBF en formato prefijo 
;; EVALUA A : T si FBF es una clausula, NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun clause-p (wff)
  (cond ((equal wff nil) nil)  
        ((not (listp wff)) nil)
        ((not (equal 'v (first wff))) nil)
        ((equal nil (rest wff)) t)
        ((equal nil (no-binary-operation (cadr wff))) nil)
        (t (clause-p (cons 'v (cddr wff)))))) 

(defun no-binary-operation (exp)
  (cond 
   ((equal (literal-p exp) t))
   (t (and (wff-prefix-p exp) (not (n-ary-connector-p (first exp)))))))
      

;;
;; EJEMPLOS:
;;
(clause-p '(v))             ; T
(clause-p '(v p))           ; T
(clause-p '(v (¬ r)))       ; T
(clause-p '(v p q (¬ r) s)) ; T
(clause-p NIL)                    ; NIL
(clause-p 'p)                     ; NIL
(clause-p '(¬ p))                 ; NIL
(clause-p NIL)                    ; NIL
(clause-p '(p))                   ; NIL
(clause-p '((¬ p)))               ; NIL
(clause-p '(^ a b q (¬ r) s))     ; NIL
(clause-p '(v (^ a b) q (¬ r) s)) ; NIL
(clause-p '(¬ (v p q)))           ; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.7
;; Predicado para determinar si una FBF esta en FNC  
;;
;; RECIBE   : FFB en formato prefijo 
;; EVALUA A : T si FBF esta en FNC con conectores, 
;;            NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cnf-p (wff)
  (cond
   ((equal wff nil) nil)
   ((not (equal '^ (first wff))) nil)
   ((equal (cadr wff) nil) t)
   ((equal nil (clause-p (cadr wff))) nil)
   (t (cnf-p (cons '^ (cddr wff))))))

;;
;; EJEMPLOS:
;;
(cnf-p '(^ (v a  b c) (v q r) (v (¬ r) s) (v a b))) ; T
(cnf-p '(^ (v a  b (¬ c)) ))                        ; T
(cnf-p '(^ ))                                       ; T
(cnf-p '(^(v )))                                    ; T
(cnf-p '(¬ p))                                      ; NIL
(cnf-p '(^ a b q (¬ r) s))                          ; NIL
(cnf-p '(^ (v a b) q (v (¬ r) s) a b))              ; NIL
(cnf-p '(v p q (¬ r) s))                            ; NIL
(cnf-p '(^ (v a b) q (v (¬ r) s) a b))              ; NIL
(cnf-p '(^ p))                                      ; NIL
(cnf-p '(v ))                                       ; NIL
(cnf-p NIL)                                         ; NIL
(cnf-p '((¬ p)))                                    ; NIL
(cnf-p '(p))                                        ; NIL
(cnf-p '(^ (p)))                                    ; NIL
(cnf-p '((p)))                                      ; NIL
(cnf-p '(^ a b q (r) s))                            ; NIL
(cnf-p '(^ (v a  (v b c)) (v q r) (v (¬ r) s) a b)) ; NIL
(cnf-p '(^ (v a (^ b c)) (^ q r) (v (¬ r) s) a b))  ; NIL
(cnf-p '(¬ (v p q)))                                ; NIL
(cnf-p '(v p q (r) s))                              ; NIL 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.1: Incluya comentarios en el codigo adjunto
;;
;; Dada una FBF, evalua a una FBF equivalente 
;; que no contiene el connector <=>
;;
;; RECIBE   : FBF en formato prefijo 
;; EVALUA A : FBF equivalente en formato prefijo 
;;            sin connector <=>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eliminate-biconditional (wff);; transforma la bicondicional a su forma equivalente con condicional
  (if (or (null wff) (literal-p wff)) ;; Si es nula o literal
      wff ;devuelve el literal
    (let ((connector (first wff))) ;; Llama connector al primer elemento de wff
      (if (eq connector +bicond+) ;; Si es efectivamente una bicondicional
          (let ((wff1 (eliminate-biconditional (second wff)))  ;; llama wff1 al elemento izquierdo de la bicondicion
                (wff2 (eliminate-biconditional (third  wff)))) ;; llama wff1 al elemento derecho de la bicondicion
            (list +and+ ;;Creamos el and externo entre las dos wff resultantes segun la formula
                  (list +cond+ wff1 wff2) ;; Creamos la condicional de wff1 a wff2 interna segun la formula
                  (list +cond+ wff2 wff1))) ;; Creamos la condicional de wff2 a wff1 interna segun la formula
        (cons connector ;; creamos los pares entre el conector y el mapcar correspondiente
              (mapcar #'eliminate-biconditional (rest wff))))))) ;; llama recursivamente a la bicondicional con el resto de elementos

;;
;; EJEMPLOS:
;;
(eliminate-biconditional '(<=> p  (v q s p) ))
;;   (^ (=> P (v Q S P)) (=> (v Q S P) P))
(eliminate-biconditional '(<=>  (<=> p  q) (^ s (Â¬ q))))
;;   (^ (=> (^ (=> P Q) (=> Q P)) (^ S (Â¬ Q)))
;;      (=> (^ S (Â¬ Q)) (^ (=> P Q) (=> Q P))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.2
;; Dada una FBF, que contiene conectores => evalua a
;; una FBF equivalente que no contiene el connector =>
;;
;; RECIBE   : wff en formato prefijo sin el connector <=> 
;; EVALUA A : wff equivalente en formato prefijo 
;;            sin el connector =>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eliminate-conditional (wff)  ;; transforma la condicional a su forma equivalente segun la regla
  (if (or (null wff) (literal-p wff)) ;; Si es nula o literal
      wff ;; devuelve el literal
    (let ((connector (first wff))) ;; Llama connector al primer elemento de wff
      (if (eq connector +cond+) ;; Si es efectivamente una condicional
          (let ((wff1 (eliminate-conditional (second wff))) ;; llama wf1 al elemento izquierdo de la condicional
                (wff2 (eliminate-conditional (third  wff)))) ;; llama wf2 al elemento derecho de la condicional
            (list +or+ ;;Creamos el or externo entre las dos wff resultantes segun la formula
                  (list +not+ wff1) ;;Negamos la wff1 interna segun la formula
                  (list wff2))) ;;Dejamos la otra wff2 interna como lista
        (cons connector ;;creamos los pares entre el conector y el mapcar correspondiente
              (mapcar #'eliminate-conditional (rest wff))))))) ;; llama recursivamente a la condicional con el resto de elementos

;;
;; EJEMPLOS:
;;
(eliminate-conditional '(=> p q))                      ;;; (V (Â¬ P) Q)
(eliminate-conditional '(=> p (v q s p)))              ;;; (V (Â¬ P) (V Q S P))
(eliminate-conditional '(=> (=> (Â¬ p) q) (^ s (Â¬ q)))) ;;; (V (Â¬ (V (Â¬ (Â¬ P)) Q)) (^ S (Â¬ Q)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.3
;; Dada una FBF, que no contiene los conectores <=>, => 
;; evalua a una FNF equivalente en la que la negacion  
;; aparece unicamente en literales negativos
;;
;; RECIBE   : FBF en formato prefijo sin conector <=>, => 
;; EVALUA A : FBF equivalente en formato prefijo en la que 
;;            la negacion  aparece unicamente en literales 
;;            negativos.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun reduce-scope-of-negation (wff)
  (if
  	(or (null wff) (connector-p wff))
  	wff
  	(de-morgan wff)))

(defun exchange-and-or (connector)
  (cond
   ((eq connector +and+) +or+)    
   ((eq connector +or+) +and+)
   (t connector)))

(defun exchange-andor-not (connector);;
  (if
   (n-ary-connector-p connector)
    +not+
    connector))



;;; PARECIDA A LAS DE ARRIBA, NO FUNCIONA
(defun de-morgan (wff)
	(if (or (null wff) (literal-p wff)) ;; Si es nula o literal
      wff ;devuelve el literal
	(let ((not-connector (first (second wff)))
		  (unary-connector (first wff)))
	(if 
	  ;(let ((connector (first (second wff))))
	  (and (unary-connector-p unary-connector) (n-ary-connector-p not-connector))
	    (let ((wff1 (de-morgan (second wff)))
	    	  (wff2 (de-morgan (third wff))))
	    	(list (exchange-and-or unary-connector)
	    		  (list +not+ wff1)
	    		  (list wff2)))
	  (cons (not-connector)
	  		  (mapcar #'de-morgan (rest wff)))))))


;;; FUNCIONA SOLO PARA EL PRIMER CASO
(defun de-morgan (wff)
	(if 
	  ;(let ((connector (first (second wff))))
	  (and (unary-connector-p (first wff)) (n-ary-connector-p (first (second wff))))
	    ;;(list exchange-and-or (first (second wff))
	    ;;	(list +not+ (de-morgan (rest (second wff)))))
	    (cons (exchange-and-or (first (second wff)))
	    	(mapcar #'(lambda (x) (put-nots (rest x))) (rest wff)))
	    ;;(cons (exchange-and-or (first (second wff))) 
;;DONTKNOW;;(list (exchange-andor-not (first (second wff))) (mapcar #'de-morgan (rest (second wff)))))
;;APARTE HAY QUE LLAMAR POR CADA MAPCAR QUE HAGAMOS A REDUCE-NOT-NOT DE ESO
	    wff))

(defun put-nots (lst)
	(combine-elem-lst +not+ lst))

	    ;;(list exchange-and-or (first (second wff))
	    ;;	(list +not+ (de-morgan (rest (second wff)))))
	   ; (cons (exchange-and-or (first (second wff)))
	    ;	(mapcar #'(lambda (x) (put-nots (de-morgan (rest x)))) (de-morgan (rest wff))))
	    ;;(cons (exchange-and-or (first (second wff))) 
;;DONTKNOW;;(list (exchange-andor-not (first (second wff))) (mapcar #'de-morgan (rest (second wff)))))
;;APARTE HAY QUE LLAMAR POR CADA MAPCAR QUE HAGAMOS A REDUCE-NOT-NOT DE ESO
	    ;wff))

;;; CREO QUE HABRA QUE HACERLA RECURSIVA TAMBIEN AUNQUE FUNCIONA
(defun reduce-not-not (wff)
  (if
  	(and (unary-connector-p (first wff)) (unary-connector-p (first (second wff)))) 
  	  (rest (second wff))
	  wff))

(defun reduce-not-not (wff)
	(if
		(unary-connector-p (first wff))
		  (if unary-connector-p (first (second wff))
		  	(rest (second wff))
		  	(reduce-not-not ()))
		  wff))

;;
;;  EJEMPLOS:
;;
(reduce-scope-of-negation '(Â¬ (v p (Â¬ q) r))) 
;;; (^ (Â¬ P) Q (Â¬ R))
(reduce-scope-of-negation '(Â¬ (^ p (Â¬ q) (v  r s (Â¬ a))))) 
;;;  (V (Â¬ P) Q (^ (Â¬ R) (Â¬ S) A))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.4: Comente el codigo adjunto 
;;
;; Dada una FBF, que no contiene los conectores <=>, => en la 
;; que la negacion aparece unicamente en literales negativos
;; evalua a una FNC equivalente en FNC con conectores ^, v  
;;
;; RECIBE   : FBF en formato prefijo sin conector <=>, =>, 
;;            en la que la negacion aparece unicamente 
;;            en literales negativos
;; EVALUA A : FBF equivalente en formato prefijo FNC 
;;            con conectores ^, v
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun combine-elt-lst (elt lst);; le pasamos un elemento y una lista y los combina
  (if (null lst);; si la lista es nula
      (list (list elt));;  devuelve ((elt))
    (mapcar #'(lambda (x) (cons elt x)) lst)));; si no, devuelve las combinaciones del elt con la lista

(defun exchange-NF (nf);; dada una forma normal, la intercambia
  (if (or (null nf) (literal-p nf)) ;; si la forma normal es nula o literal
      nf ;;la devuelve tal cual
    (let ((connector (first nf))) ;; llamamos conector al primer elemento de la forma normal
      (cons (exchange-and-or connector) ;; hacemos una lista intercambiando el conector como primer elemento
            (mapcar #'(lambda (x) ;; como segundo elemento, hacemos un mapcar
                          (cons connector x)) ;; de la sublista formada por el conector y el resultado de
                (exchange-NF-aux (rest nf)))))));;; llamar a intercambiar NF aux con toda la nf menos el conector primero

(defun exchange-NF-aux (nf) ;; dada una forma normal, 
  (if (null nf) ;; si la nf es nula 
      NIL ;; devuelve nil
    (let ((lst (first nf))) ;; llamamos lst al primer elemento de la nf
      (mapcan #'(lambda (x)  ;; realizamos un mapcar eliminando los nil
                  (combine-elt-lst  
                   x ;; combina el elt resultante del if de abajo
                   (exchange-NF-aux (rest nf)))) ;; con el resto de la nf
        (if (literal-p lst) (list lst) (rest lst))))));; si lst es un literal, lo hacemos lista y si no, pasamos el resto de la lst

(defun simplify (connector lst-wffs );; simplifica un conector y unas listas de fbf prefijo
  (if (literal-p lst-wffs);; si son un literal
      lst-wffs;; devuelve la lista de fbf prefijo tal cual                    
    (mapcan #'(lambda (x) ;;; hacemos un mapcar eliminando nils
                (cond 
                 ((literal-p x) (list x)) ;;; si alguna sublista es literal, la hace lista y la devuelve
                 ((equal connector (first x)) ;;; si el primer elemento de la sublista es el conector a simplificar
                  (mapcan  ;;;hacemos un mapcar eliminando nils
                      #'(lambda (y) (simplify connector (list y)))  ;;;llamamos recursivamente a simplificar con el resto de sublistas
                    (rest x))) 
                 (t (list x)))) ;;; si no entra en ninguna de las dos condiciones anteriores, devuelve la sublista               
      lst-wffs)))

(defun cnf (wff) ;; traduce una FBF prefijo en FNC apoyandose en las funciones auxiliares anteriores
  (cond
   ((cnf-p wff) wff) ;;si la FBF estÃ¡ ya en FNC la devuelve
   ((literal-p wff) ;; si la FBF es un literal
    (list +and+ (list +or+ wff))) ;; devuelve (v (^ wff))
   ((let ((connector (first wff))) ;; llamamos conector al primer elemento de la wff 
      (cond
       ((equal +and+ connector)  ;; si el conector es un v 
        (cons +and+ (simplify +and+ (mapcar #'cnf (rest wff))))) ;;devuelve una lista simplificada con ands 
       ((equal +or+ connector) ;; si el conector es un ^
        (cnf (exchange-NF (cons +or+ (simplify +or+ (rest wff))))))))))) ;;devuelve una lista simplificada con ors


(cnf 'a)

(cnf '(v (Â¬ a) b c))
(print (cnf '(^ (v (Â¬ a) b c) (Â¬ e) (^ e f (Â¬ g) h) (v m n) (^ r s q) (v u q) (^ x y))))
(print (cnf '(v (^ (Â¬ a) b c) (Â¬ e) (^ e f (Â¬ g) h) (v m n) (^ r s q) (v u q) (^ x y))))
(print (cnf '(^ (v p  (Â¬ q)) a (v k  r  (^ m  n)))))
(print (cnf '(v p  q  (^ r  m)  (^ n  a)  s )))
(exchange-NF '(v p  q  (^ r  m)  (^ n  a)  s ))
(cnf '(^ (v a b (^ y r s) (v k l)) c (Â¬ d) (^ e f (v h i) (^ o p))))
(cnf '(^ (v a b (^ y r s)) c (Â¬ d) (^ e f (v h i) (^ o p))))
(cnf '(^ (^ y r s (^ p q (v c d))) (v a b)))
(print (cnf '(^ (v (Â¬ a) b c) (Â¬ e) r s 
                (v e f (Â¬ g) h) k (v m n) d)))
;;
(cnf '(^ (v p (Â¬ q)) (v k r (^ m  n))))
(print  (cnf '(v (v p q) e f (^ r  m) n (^ a (Â¬ b) c) (^ d s))))
(print (cnf '(^ (^ (Â¬ y) (v r (^ s (Â¬ x)) (^ (Â¬ p) m (v c d))) (v (Â¬ a) (Â¬ b))) g)))
;;
;; EJEMPLOS:
;;
(cnf NIL)              ; NIL
(cnf 'a)               ; (^ (V A))
(cnf '(Â¬ a))           ; (^ (V (Â¬ A)))
(cnf '(V (Â¬ P) (Â¬ P))) ; (^ (V (Â¬ P) (Â¬ P)))
(cnf '(V A))           ; (^ (V A))
(cnf '(^ (v p (Â¬ q)) (v k r (^ m  n))))
;;;   (^ (V P (Â¬ Q)) (V K R M) (V K R N))
(print  (cnf '(v (v p q) e f (^ r  m) n (^ a (Â¬ b) c) (^ d s))))
;;; (^ (V P Q E F R N A D)      (V P Q E F R N A S)
;;;    (V P Q E F R N (Â¬ B) D)  (V P Q E F R N (Â¬ B) S)
;;;    (V P Q E F R N C D)      (V P Q E F R N C S) 
;;;    (V P Q E F M N A D)      (V P Q E F M N A S) 
;;;    (V P Q E F M N (Â¬ B) D)  (V P Q E F M N (Â¬ B) S) 
;;;    (V P Q E F M N C D)      (V P Q E F M N C S))
;;;
(print 
 (cnf '(^ (^ (Â¬ y) (v r (^ s (Â¬ x)) 
                      (^ (Â¬ p) m (v c d)))(v (Â¬ a) (Â¬ b))) g)))
;;;(^ (V (Â¬ Y)) (V R S (Â¬ P)) (V R S M) 
;;;   (V R S C D) (V R (Â¬ X) (Â¬ P)) 
;;;   (V R (Â¬ X) M) (V R (Â¬ X) C D)
;;;   (V (Â¬ A) (Â¬ B)) (V G))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.5:
;;
;; Dada una FBF en  FNC
;; evalua a lista de listas sin conectores
;; que representa una conjuncion de disyunciones de literales
;;
;; RECIBE   : FBF en FNC con conectores ^, v
;; EVALUA A : FBF en FNC (con conectores ^, v eliminaos)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun is-atom (x)
	(if (or (truth-value-p x) (connector-p x) (literal-p x)) 
		T nil))

(defun eliminando-bien (cnf)
  (if (literal-p cnf)
      cnf                    
    (mapcan #'(lambda (x)
                (cond 
                 ((literal-p x) (list x))
                 ((n-ary-connector-p (first x)) ;;; si el primer elemento de la sublista es el n-ario a eliminar
                  (mapcan  ;;;hacemos un mapcar eliminando nils
                      #'(lambda (y) (eliminando-bien (list y)))  ;;;llamamos recursivamente a simplificar con el resto de sublistas
                    (rest x))) 
                 (t (list x)))) ;;; si no entra en ninguna de las dos condiciones anteriores, devuelve la sublista               
      cnf)))

(eliminate-connectors 'nil)
(eliminate-connectors (cnf '(^ (v p  (Â¬ q))  (v k  r  (^ m  n)))))
(eliminate-connectors
 (cnf '(^ (v (Â¬ a) b c) (Â¬ e) (^ e f (Â¬ g) h) (v m n) (^ r s q) (v u q) (^ x y))))

(eliminate-connectors (cnf '(v p  q  (^ r  m)  (^ n  q)  s )))
(eliminate-connectors (print (cnf '(^ (v p  (Â¬ q)) (Â¬ a) (v k  r  (^ m  n))))))

(eliminate-connectors '(^))
(eliminate-connectors '(^ (v p (Â¬ q)) (v) (v k r)))
(eliminate-connectors '(^ (v a b)))

;;   EJEMPLOS:
;;

(eliminate-connectors '(^ (v p (Â¬ q)) (v k r)))
;; ((P (Â¬ Q)) (K R))
(eliminate-connectors '(^ (v p (Â¬ q)) (v q (Â¬ a)) (v s e f) (v b)))
;; ((P (Â¬ Q)) (Q (Â¬ A)) (S E F) (B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.6
;; Dada una FBF en formato infijo
;; evalua a lista de listas sin conectores
;; que representa la FNC equivalente
;;
;; RECIBE   : FBF 
;; EVALUA A : FBF en FNC (con conectores ^, v eliminados)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun wff-infix-to-cnf (wff)
  (eliminate-connectors  
    (cnf
      (infix-to-prefix wff))))

;;
;; EJEMPLOS:
;; 
(wff-infix-to-cnf 'a)
(wff-infix-to-cnf '(Â¬ a))
(wff-infix-to-cnf  '( (Â¬ p) v q v (Â¬ r) v (Â¬ s)))
(wff-infix-to-cnf  '((p v (a => (b ^ (Â¬ c) ^ d))) ^ ((p <=> (Â¬ q)) ^ p) ^ e))
;; ((P (Â¬ A) B) (P (Â¬ A) (Â¬ C)) (P (Â¬ A) D) ((Â¬ P) (Â¬ Q)) (Q P) (P) (E))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.1
;; eliminacion de literales repetidos una clausula 
;; 
;; RECIBE   : K - clausula (lista de literales, disyuncion implicita)
;; EVALUA A : clausula equivalente sin literales repetidos 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eliminate-repeated-literals (k)
  (cond 
   ((equal k nil ) nil)
   ((equal (find (first k) (rest k) :test #'equal) nil) (cons (first k) (eliminate-repeated-literals (rest k))))
   (t (eliminate-repeated-literals (rest k)))))

;;
;; EJEMPLO:
;;
(trace eliminate-repeated-literals)
(eliminate-repeated-literals '(a b))
(eliminate-repeated-literals '(a (¬ c) (¬ c) b))
(eliminate-repeated-literals '(a b c (¬ a) a c (¬ c) c a))
;;;   (B (¬ A) (¬ C) C A)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.2
;; eliminacion de clausulas repetidas en una FNC 
;; 
;; RECIBE   : cnf - FBF en FNC (lista de clausulas, conjuncion implicita)
;; EVALUA A : FNC equivalente sin clausulas repetidas 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defun eliminate-repeated-clauses (cnf)
  (let ((operador (eliminate-repeated-literals (first cnf))))
  (cond 
   ((equal cnf nil) nil)
   ((equal (find operador (rest cnf) :test #'our-equal) nil) (cons operador (eliminate-repeated-clauses (rest cnf))))
   (t (eliminate-repeated-clauses (rest cnf))))))

(eliminate-repeated-clauses '(((¬ a) c) (c (¬ a)) ((¬ a) (¬ a) b c b) (a a b) (c (¬ a) b  b) (a b)))
;;; ((C (¬ A)) (C (¬ A) B) (A B))
    
    
  
(defun contenido (l1 l2)
  (if (equal nil l1)
      t
    (and (not (equal nil (find (first l1) (eliminate-repeated-literals l2) :test #'equal)))
         (contenido (rest l1) l2))))

(defun our-equal (l1 l2)
  (and (contenido l1 l2) (contenido l2 l1)))

;;
;; EJEMPLO:
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.3
;; Predicado que determina si una clausula subsume otra
;;
;; RECIBE   : K1, K2 clausulas
;; EVALUA a : K1 si K1 subsume a K2
;;            NIL en caso contrario
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun subsume (K1 K2)
  (if (equal t (contenido k1 k2))
      k1
    nil))
  
;;
;;  EJEMPLOS:
;;
(subsume '(a) '(a b (¬ c)))
;; ((a))
(subsume NIL '(a b (¬ c)))
;; (NIL)
(subsume '(a b (¬ c)) '(a) )
;; NIL
(subsume '( b (¬ c)) '(a b (¬ c)))
;; ( b (¬ c))
(subsume '(a b (¬ c)) '( b (¬ c)))
;; NIL
(subsume '(a b (¬ c)) '(d  b (¬ c)))
;; nil
(subsume '(a b (¬ c)) '((¬ a) b (¬ c) a))
;; (A B (¬ C))
(subsume '((¬ a) b (¬ c) a) '(a b (¬ c)) )
;; nil

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.4
;; eliminacion de clausulas subsumidas en una FNC 
;; 
;; RECIBE   : K (clausula), cnf (FBF en FNC)
;; EVALUA A : FBF en FNC equivalente a cnf sin clausulas subsumidas 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eliminate-subsumed-clauses (cnf) 
  (cond
   ((equal nil cnf) nil)
   

;;
;;  EJEMPLOS:
;;
(eliminate-subsumed-clauses 
 '((a b c) (b c) (a (¬ c) b)  ((¬ a) b) (a b (¬ a)) (c b a)))
;;; ((A (¬ C) B) ((¬ A) B) (B C)) ;; el orden no es importante
(eliminate-subsumed-clauses
 '((a b c) (b c) (a (¬ c) b) (b)  ((¬ a) b) (a b (¬ a)) (c b a)))
;;; ((B))
(eliminate-subsumed-clauses
 '((a b c) (b c) (a (¬ c) b) ((¬ a))  ((¬ a) b) (a b (¬ a)) (c b a)))
;;; ((A (¬ C) B) ((¬ A)) (B C))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.5
;; Predicado que determina si una clausula es tautologia
;;
;; RECIBE   : K (clausula)
;; EVALUA a : T si K es tautologia
;;            NIL en caso contrario
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tautology-p (k) 
  (let ((op (first k)))
    (cond 
     ((equal k nil) nil)
     ((and
       (equal (positive-literal-p op) t)
       (not (equal (find (cons '¬ (list (first k))) k :test #'equal) nil)))
      nil)
     ((and
        (equal (negative-literal-p op) t)
       (not (equal(find (second (first k)) k :test #'equal) nil)))
      nil)
     (t (and t (tautology-p (rest k)))))))

    
  
(find (exp2 k :test #'equal))))))




;;
;;  EJEMPLOS:
;;
(tautology-p '(B A))
(tautology-p '((¬ B) A C (¬ A) D)) ;;; T 
(tautology-p '((¬ B) A C D))       ;;; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.6
;; eliminacion de clausulas en una FBF en FNC que son tautologia
;;
;; RECIBE   : cnf - FBF en FNC
;; EVALUA A : FBF en FNC equivalente a cnf sin tautologias 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eliminate-tautologies (cnf) 
  ;;
  ;; 4.3.6 Completa el codigo
  ;;
  )

;;
;;  EJEMPLOS:
;;
(eliminate-tautologies 
 '(((¬ b) a) (a (¬ a) b c) ( a (¬ b)) (s d (¬ s) (¬ s)) (a)))
;; (((¬ B) A) (A (¬ B)) (A))

(eliminate-tautologies '((a (¬ a) b c)))
;; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.7
;; simplifica FBF en FNC 
;;        * elimina literales repetidos en cada una de las clausulas 
;;        * elimina clausulas repetidas
;;        * elimina tautologias
;;        * elimina clausulass subsumidas
;;  
;; RECIBE   : cnf  FBF en FNC
;; EVALUA A : FNC equivalente sin clausulas repetidas, 
;;            sin literales repetidos en las clausulas
;;            y sin clausulas subsumidas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun simplify-cnf (cnf) 
  ;;
  ;; 4.3.7 Completa el codigo
  ;;
  )

;;
;;  EJEMPLOS:
;;
(simplify-cnf '((a a) (b) (a) ((¬ b)) ((¬ b)) (a b c a)  (s s d) (b b c a b)))
;; ((B) ((¬ B)) (S D) (A)) ;; en cualquier orden

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.1
;; Construye el conjunto de clausulas lambda-neutras para una FNC 
;;
;; RECIBE   : cnf    - FBF en FBF simplificada
;;            lambda - literal positivo
;; EVALUA A : cnf_lambda^(0) subconjunto de clausulas de cnf  
;;            que no contienen el literal lambda ni Â¬lambda   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FUNCIONA PERFECTO, PREGUNTAR SI SE PUEDE CAMBIAR DE LAMBDA A LAMDA Y DE CNF A FNC, CAMBIAR EL ORDEN DEL CONS

(defun extract-neutral-clauses (lamda cnf) 
  (intersection (extract-no-positive-clauses lamda cnf) (extract-no-negative-clauses lamda cnf)))

(defun extract-no-positive-clauses (lamda cnf) 
  (if (or (null cnf) (literal-p cnf))
  	NIL
  	(if (equal (member lamda (first cnf) :test #'equal) NIL)
  		(cons (first cnf) (extract-no-positive-clauses lamda (rest cnf)))
  		(extract-no-positive-clauses lamda (rest cnf)))))

(defun extract-no-negative-clauses (lamda cnf) 
  (if (or (null cnf) (literal-p cnf))
  	NIL
  	(if (equal (member (list +not+ lamda) (first cnf) :test #'equal) NIL)
  		(cons (first cnf) (extract-no-negative-clauses lamda (rest cnf)))
  		(extract-no-negative-clauses lamda (rest cnf)))))

;;
;;  EJEMPLOS:
;;
(extract-neutral-clauses 'p
                           '((p (Â¬ q) r) (p q) (r (Â¬ s) q) (a b p) (a (Â¬ p) c) ((Â¬ r) s)))
;; ((R (Â¬ S) Q) ((Â¬ R) S))


(extract-neutral-clauses 'r NIL)
;; NIL

(extract-neutral-clauses 'r '(NIL))
;; (NIL)

(extract-neutral-clauses 'r
                           '((p (Â¬ q) r) (p q) (r (Â¬ s) q) (a b p) (a (Â¬ p) c) ((Â¬ r) s)))
;; ((P Q) (A B P) (A (Â¬ P) C))

(extract-neutral-clauses 'p
                           '((p (Â¬ q) r) (p q) (r (Â¬ s) p q) (a b p) (a (Â¬ p) c) ((Â¬ r) p s)))
;; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.2
;; Construye el conjunto de clausulas lambda-positivas para una FNC
;;
;; RECIBE   : cnf    - FBF en FNC simplificada
;;            lambda - literal positivo
;; EVALUA A : cnf_lambda^(+) subconjunto de clausulas de cnf 
;;            que contienen el literal lambda  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun extract-positive-clauses (lamda cnf) 
  (if (or (null cnf) (literal-p cnf))
  	NIL
  	(if (equal (member lamda (first cnf) :test #'equal) NIL)
  		(extract-positive-clauses lamda (rest cnf))
  		(cons (first cnf) (extract-positive-clauses lamda (rest cnf))))))


;;
;;  EJEMPLOS:
;;
(extract-positive-clauses 'p
                             '((p (Â¬ q) r) (p q) (r (Â¬ s) q) (a b p) (a (Â¬ p) c) ((Â¬ r) s)))

;; ((P (Â¬ Q) R) (P Q) (A B P))


(extract-positive-clauses 'r NIL)
;; NIL
(extract-positive-clauses 'r '(NIL))
;; NIL
(extract-positive-clauses 'r
                             '((p (Â¬ q) r) (p q) (r (Â¬ s) q) (a b p) (a (Â¬ p) c) ((Â¬ r) s)))
;; ((P (Â¬ Q) R) (R (Â¬ S) Q))
(extract-positive-clauses 'p
                             '(((Â¬ p) (Â¬ q) r) ((Â¬ p) q) (r (Â¬ s) (Â¬ p) q) (a b (Â¬ p)) ((Â¬ r) (Â¬ p) s)))
;; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.3
;; Construye el conjunto de clausulas lambda-negativas para una FNC 
;;
;; RECIBE   : cnf    - FBF en FNC simplificada
;;            lambda - literal positivo 
;; EVALUA A : cnf_lambda^(-) subconjunto de clausulas de cnf  
;;            que contienen el literal Â¬lambda  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun extract-negative-clauses (lamda cnf) 
  (if (or (null cnf) (literal-p cnf))
  	NIL
  	(if (equal (member (list +not+ lamda) (first cnf) :test #'equal) NIL)
  		(extract-negative-clauses lamda (rest cnf))
  		(cons (first cnf) (extract-negative-clauses lamda (rest cnf))))))

;;
;;  EJEMPLOS:
;;
(extract-negative-clauses 'p
                             '((p (Â¬ q) r) (p q) (r (Â¬ s) q) (a b p) (a (Â¬ p) c) ((Â¬ r) s)))
;; ((A (Â¬ P) C))

(extract-negative-clauses 'r NIL)
;; NIL
(extract-negative-clauses 'r '(NIL))
;; NIL
(extract-negative-clauses 'r
                             '((p (Â¬ q) r) (p q) (r (Â¬ s) q) (a b p) (a (Â¬ p) c) ((Â¬ r) s)))
;; (((Â¬ R) S))
(extract-negative-clauses 'p
                             '(( p (Â¬ q) r) ( p q) (r (Â¬ s) p q) (a b p) ((Â¬ r) p s)))
;; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.4
;; resolvente de dos clausulas
;;
;; RECIBE   : lambda      - literal positivo
;;            K1, K2      - clausulas simplificadas
;; EVALUA A : res_lambda(K1,K2) 
;;                        - lista que contiene la 
;;                          clausula que resulta de aplicar resolucion 
;;                          sobre K1 y K2, con los literales repetidos 
;;                          eliminados
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; En k1 esta el lamda y en k2 el no lamda o viceversa
;; SI pasa devuelves union k1 menos landa y k2 menos no lamda y elimina repitidos

(defun resolve-on (lamda K1 K2)
	(if (or (K1 NULL) (K2 NULL))
		NIL
		(cond 
			((and (member lamda K1 :test #'equal) (member (list +not+ lamda) K2)) 
				(eliminate-repeated-literals (union (remove lamda K1) (remove (list +not+ lamda) K2))))
			((and (member (list +not+ lamda) K1 :test #'equal) (member lamda K2 :test #'equal))
				(eliminate-repeated-literals (union (remove (list +not+ lamda) K1) (remove lamda K2))))
			((t) nil))))
;;
;;  EJEMPLOS:
;;
(resolve-on 'p '(a b (Â¬ c) p) '((Â¬ p) b a q r s))
;; (((Â¬ C) B A Q R S))

(resolve-on 'p '(a b (Â¬ c) (Â¬ p)) '( p b a q r s))
;; (((Â¬ C) B A Q R S))

(resolve-on 'p '(p) '((Â¬ p)))
;; (NIL)


(resolve-on 'p NIL '(p b a q r s))
;; NIL

(resolve-on 'p NIL NIL)
;; NIL

(resolve-on 'p '(a b (Â¬ c) (Â¬ p)) '(p b a q r s))
;; (((Â¬ C) B A Q R S))

(resolve-on 'p '(a b (Â¬ c)) '(p b a q r s))
;; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.5
;; Construye el conjunto de clausulas RES para una FNC 
;;
;; RECIBE   : lambda - literal positivo
;;            cnf    - FBF en FNC simplificada
;;            
;; EVALUA A : RES_lambda(cnf) con las clauses repetidas eliminadas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun build-RES (lamda cnf)
  (union (extract-neutral-clauses lamda cnf) (resolve-on lamda (extract-positive-clauses lamda cnf) (extract-negative-clauses lamda cnf))))

;;
;;  EJEMPLOS:
;;
(build-RES 'p NIL)
;; NIL
(build-RES 'P '((A  (Â¬ P) B) (A P) (A B)));; ((A B))
(build-RES 'P '((B  (Â¬ P) A) (A P) (A B)));; ((B A))

(build-RES 'p '(NIL))
;; (NIL)

(build-RES 'p '((p) ((Â¬ p))))
;; (NIL)

(build-RES 'q '((p q) ((Â¬ p) q) (a b q) (p (Â¬ q)) ((Â¬ p) (Â¬ q))))
;; ((P) ((Â¬ P) P) ((Â¬ P)) (B A P) (B A (Â¬ P)))

(build-RES 'p '((p q) (c q) (a b q) (p (Â¬ q)) (p (Â¬ q))))
;; ((A B Q) (C Q))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.5
;; Construye el conjunto de clausulas RES para una FNC 
;;
;; RECIBE   : lambda - literal positivo
;;            cnf    - FBF en FNC simplificada
;;            
;; EVALUA A : RES_lambda(cnf) con las clauses repetidas eliminadas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun build-RES (lambda cnf)
  ;;
  ;; 4.4.5 Completa el codigo
  ;;
)

;;
;;  EJEMPLOS:
;;
(build-RES 'p NIL)
;; NIL
(build-RES 'P '((A  (¬ P) B) (A P) (A B)));; ((A B))
(build-RES 'P '((B  (¬ P) A) (A P) (A B)));; ((B A))

(build-RES 'p '(NIL))
;; (NIL)

(build-RES 'p '((p) ((¬ p))))
;; (NIL)

(build-RES 'q '((p q) ((¬ p) q) (a b q) (p (¬ q)) ((¬ p) (¬ q))))
;; ((P) ((¬ P) P) ((¬ P)) (B A P) (B A (¬ P)))

(build-RES 'p '((p q) (c q) (a b q) (p (¬ q)) (p (¬ q))))
;; ((A B Q) (C Q))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.5
;; Comprueba si una FNC es SAT calculando RES para todos los
;; atomos en la FNC 
;;
;; RECIBE   : cnf - FBF en FNC simplificada
;; EVALUA A :	T  si cnf es SAT
;;                NIL  si cnf es UNSAT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun  RES-SAT-p (cnf) 
  ;;
  ;; 4.5 Completa el codigo
  ;;
  )

;;
;;  EJEMPLOS:
;;
;;
;; SAT Examples
;;
(RES-SAT-p nil)  ;;; T
(RES-SAT-p '((p) ((¬ q)))) ;;; T 
(RES-SAT-p
 '((a b d) ((¬ p) q) ((¬ c) a b) ((¬ b) (¬ p) d) (c d (¬ a)))) ;;; T 
(RES-SAT-p
 '(((¬ p) (¬ q) (¬ r)) (q r) ((¬ q) p) ((¬ q)) ((¬ p) (¬ q) r))) ;;;T
;;
;; UNSAT Examples
;;
(RES-SAT-p '(nil))         ;;; NIL
(RES-SAT-p '((S) nil))     ;;; NIL 
(RES-SAT-p '((p) ((¬ p)))) ;;; NIL
(RES-SAT-p
 '(((¬ p) (¬ q) (¬ r)) (q r) ((¬ q) p) (p) (q) ((¬ r)) ((¬ p) (¬ q) r))) ;;; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.6:
;; Resolucion basada en RES-SAT-p
;;
;; RECIBE   : wff - FBF en formato infijo 
;;            w   - FBF en formato infijo 
;;                               
;; EVALUA A : T   si w es consecuencia logica de wff
;;            NIL en caso de que no sea consecuencia logica.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun logical-consequence-RES-SAT-p (wff w)
  ;;
  ;; 4.6 Completa el codigo
  ;;
  )

;;
;;  EJEMPLOS:
;;
(logical-consequence-RES-SAT-p NIL 'a) ;;; NIL
(logical-consequence-RES-SAT-p NIL NIL) ;;; NIL
(logical-consequence-RES-SAT-p '(q ^ (¬ q)) 'a) ;;; T 
(logical-consequence-RES-SAT-p '(q ^ (¬ q)) '(¬ a)) ;;; T 

(logical-consequence-RES-SAT-p '((p => (¬ p)) ^ p) 'q)
;; T

(logical-consequence-RES-SAT-p '((p => (¬ p)) ^ p) '(¬ q))
;; T

(logical-consequence-RES-SAT-p '((p => q) ^ p) 'q)
;; T

(logical-consequence-RES-SAT-p '((p => q) ^ p) '(¬q))
;; NIL

(logical-consequence-RES-SAT-p 
 '(((¬ p) => q) ^ (p => (a v (¬ b))) ^ (p => ((¬ a) ^ b)) ^ ( (¬ p) => (r  ^ (¬ q)))) 
 '(¬ a))
;; T

(logical-consequence-RES-SAT-p 
 '(((¬ p) => q) ^ (p => (a v (¬ b))) ^ (p => ((¬ a) ^ b)) ^ ( (¬ p) => (r  ^ (¬ q)))) 
 'a)
;; T

(logical-consequence-RES-SAT-p 
 '(((¬ p) => q) ^ (p => ((¬ a) ^ b)) ^ ( (¬ p) => (r  ^ (¬ q)))) 
 'a)
;; NIL

(logical-consequence-RES-SAT-p 
 '(((¬ p) => q) ^ (p => ((¬ a) ^ b)) ^ ( (¬ p) => (r  ^ (¬ q)))) 
 '(¬ a))
;; T

(logical-consequence-RES-SAT-p 
 '(((¬ p) => q) ^ (p <=> ((¬ a) ^ b)) ^ ( (¬ p) => (r  ^ (¬ q)))) 
 'q)
;; NIL

(logical-consequence-RES-SAT-p 
 '(((¬ p) => q) ^ (p <=> ((¬ a) ^ b)) ^ ( (¬ p) => (r  ^ (¬ q)))) 
 '(¬ q))
;; NIL

(or 
 (logical-consequence-RES-SAT-p '((p => q) ^ p) '(¬q))      ;; NIL
 (logical-consequence-RES-SAT-p 
  '(((¬ p) => q) ^ (p => ((¬ a) ^ b)) ^ ( (¬ p) => (r  ^ (¬ q)))) 
  'a) ;; NIL
 (logical-consequence-RES-SAT-p 
  '(((¬ p) => q) ^ (p <=> ((¬ a) ^ b)) ^ ( (¬ p) => (r  ^ (¬ q)))) 
  'q) ;; NIL
 (logical-consequence-RES-SAT-p 
  '(((¬ p) => q) ^ (p <=> ((¬ a) ^ b)) ^ ( (¬ p) => (r  ^ (¬ q)))) 
  '(¬ q)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    EJERCICIO 5                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 5.3:
;; Algoritmo BFS (Breadth-first-search) en grafos
;;
;; RECIBE   : end - F 
;;            queue   - F 
;;            net - F
;;                   
;; EVALUA A : T   si w es consecuencia logica de wff
;;            NIL en caso de que no sea consecuencia logica.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Breadth-first-search in graphs
;;;
(defun bfs (end queue net)
	(if (null queue) ’()
		(let* ((path (first queue))
			(node (first path)))
		(if (eql node end)
			(reverse path)
		  (bfs end
		  	(append (rest queue)
		  		(new-paths path node net))
		  	net)))))

(defun new-paths (path node net)
	(mapcar #’(lambda(n)
		(cons n path))
	  (rest (assoc node net))))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 5.8:
;; Algoritmo BFS Modificado el error
;;
;; RECIBE   : end - F 
;;            queue   - F 
;;            net - F
;;                   
;; EVALUA A : T   si w es consecuencia logica de wff
;;            NIL en caso de que no sea consecuencia logica.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Breadth-first-search in graphs
;;;
(defun bfs (end queue net)
	(if (null queue) ’()
		(let* ((path (first queue))
			(node (first path)))
		(if (eql node end)
			(reverse path)
		  (bfs end
		  	(append (rest queue)
		  		(new-paths path node net))
		  	net)))))

(defun new-paths (path node net)
	(mapcar #’(lambda(n)
		(cons n path))
	  (rest (assoc node net))))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;