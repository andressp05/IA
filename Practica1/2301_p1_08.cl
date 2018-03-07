
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

(defconstant +bicond+ '<=>)         ;; Definicion del simbolo del bicondicional
(defconstant +cond+   '=>)          ;; Definicion del simbolo del condicional
(defconstant +and+    '^)           ;; Definicion del simbolo del and
(defconstant +or+     'v)           ;; Definicion del simbolo del or
(defconstant +not+    '~)           ;; Definicion del simbolo del not

(defun truth-value-p (x)            ;; Funcion que devuelve true si es un valor de verdad
  (or (eql x T) (eql x NIL)))

(defun unary-connector-p (x)        ;; Funcion que devuelve true si es un not
  (eql x +not+))

(defun binary-connector-p (x)       ;; Funcion que devuelve true si es una bicondicional o conicional
  (or (eql x +bicond+) 
      (eql x +cond+)))

(defun n-ary-connector-p (x)        ;; Funcion que devuelve true si es un and o un or
  (or (eql x +and+) 
      (eql x +or+)))

(defun connector-p (x)              ;; Funcion que devuelve true si es un conector de cualquier tipo de los anteriores
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
  (and (atom x)                     ;; Vemos que es un atomo 
       (not (truth-value-p x))      ;; y que no es ni un valor de verdad
       (not (connector-p x))))      ;; ni un conector

;; EJEMPLOS:
(positive-literal-p 'p)
;; evalua a T
(positive-literal-p T)
(positive-literal-p NIL)
(positive-literal-p '~)
(positive-literal-p '=>)
(positive-literal-p '(p))
(positive-literal-p '(~ p))
(positive-literal-p '(~ (v p q)))
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
   (listp x)                                 ;; Vemos que es una lista
   (null  (cddr x))                          ;; comprobamos que tiene dos elementos
   (unary-connector-p (first x))             ;; El primero es un concetor unario
   (positive-literal-p (first (rest x)))))   ;;El segundo un literal positivo

;; EJEMPLOS:
(negative-literal-p '(~ p))        ; T
(negative-literal-p NIL)           ; NIL
(negative-literal-p '~)            ; NIL
(negative-literal-p '=>)           ; NIL
(negative-literal-p '(p))          ; NIL
(negative-literal-p '((~ p)))      ; NIL
(negative-literal-p '(~ T))        ; NIL
(negative-literal-p '(~ NIL))      ; NIL
(negative-literal-p '(~ =>))       ; NIL
(negative-literal-p 'p)            ; NIL
(negative-literal-p '((~ p)))      ; NIL
(negative-literal-p '(~ (v p q)))  ; NIL


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.3
;; Predicado para determinar si una expresion es un literal  
;;
;; RECIBE   : expresion x  
;; EVALUA A : T si la expresion es un literal, 
;;            NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun literal-p (x) 
  (or (positive-literal-p x)      ;;Miramos si es literal positivo o 
      (negative-literal-p x)))    ;; si es literal negativo

;; EJEMPLOS:
(literal-p 'p)             
(literal-p '(~ p))      
;;; evaluan a T
(literal-p '(p))
(literal-p '(~ (v p q)))
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
(wff-prefix-p '(^ (~ B)))
(wff-prefix-p '(v A (~ B)))
(wff-prefix-p '(v (~ B) A ))
(wff-prefix-p '(^ (V P (=> A (^ B (~ C) D))) (^ (<=> P (~ Q)) P) E))
;;; evaluan a T
(wff-prefix-p 'NIL)
(wff-prefix-p '(~))
(wff-prefix-p '(=>))
(wff-prefix-p '(<=>))
(wff-prefix-p '(^ (V P (=> A ( B ^ (~ C) ^ D))) (^ (<=> P (~ Q)) P) E))
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
  (unless (null x)  ;;NIl no es un FBF en formato infijo
    (or (literal-p x)  ;; Unliteral si que está en formato prefijo
        (and (listp x) ;; Si es una lista tenemos los siguientes casos:
             (let ((op1 (car x))
                   (exp1 (cadr x))
                   (list_exp2 (cddr x)))
               (cond 
                ((unary-connector-p op1)   ;; Si el concector es unario comprobamos que el segundo elemento de la lista está en formato infijo
                 (and (null list_exp2)
                      (wff-infix-p exp1)))
                ((n-ary-connector-p op1)   ;; Por convención añadimos expresiones de la forma (^) o (v)
                 (null (rest x)))
                ((binary-connector-p exp1) ;;;; Si el conector es binario, tiene que tener la estructura <op1 concetor op2>
                 (and (wff-infix-p op1)
                      (null (cdr list_exp2))
                      (wff-infix-p (car list_exp2))))
                ((n-ary-connector-p exp1) ;;Si el operador es n-ario comprobamos que op1 está en formato prefijo y el reso cumple la funcion verify
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
(wff-infix-p 'a)            ; T
(wff-infix-p '(^))          ; T  ;; por convencion
(wff-infix-p '(v))          ; T  ;; por convencion
(wff-infix-p '(A ^ (v)))            ; T  
(wff-infix-p '( a ^ b ^ (p v q) ^ (~ r) ^ s))   ; T 
(wff-infix-p '(A => B))         ; T
(wff-infix-p '(A => (B <=> C)))       ; T
(wff-infix-p '( B => (A ^ C ^ D)))      ; T   
(wff-infix-p '( B => (A ^ C)))      ; T 
(wff-infix-p '( B ^ (A ^ C)))       ; T 
(wff-infix-p '((p v (a => (b ^ (~ c) ^ d))) ^ ((p <=> (~ q)) ^ p ) ^ e))  ; T 
(wff-infix-p nil)           ; NIL
(wff-infix-p '(a ^))          ; NIL
(wff-infix-p '(^ a))          ; NIL
(wff-infix-p '(a))          ; NIL
(wff-infix-p '((a)))              ; NIL
(wff-infix-p '((a) b))              ; NIL
(wff-infix-p '(^ a b q (~ r) s))            ; NIL 
(wff-infix-p '( B => A C))            ; NIL   
(wff-infix-p '( => A))              ; NIL   
(wff-infix-p '(A =>))               ; NIL   
(wff-infix-p '(A => B <=> C))           ; NIL
(wff-infix-p '( B => (A ^ C v D)))          ; NIL   
(wff-infix-p '( B ^ C v D ))            ; NIL 
(wff-infix-p '((p v (a => e (b ^ (~ c) ^ d))) ^ ((p <=> (~ q)) ^ p ) ^ e)); NIL 

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
(prefix-to-infix '(v))          ; (V)
(prefix-to-infix '(^))          ; (^)
(prefix-to-infix '(v a))        ; A
(prefix-to-infix '(^ a))        ; A
(prefix-to-infix '(^ (~ a)))    ; (~ a)
(prefix-to-infix '(v a b))      ; (A v B)
(prefix-to-infix '(v a b c))    ; (A V B V C)
(prefix-to-infix '(^ (V P (=> A (^ B (~ C) D))) (^ (<=> P (~ Q)) P) E))
;;; ((P V (A => (B ^ (~ C) ^ D))) ^ ((P <=> (~ Q)) ^ P) ^ E)
(prefix-to-infix '(^ (v p (=> a (^ b (~ c) d))))) ; (P V (A => (B ^ (~ C) ^ D)))
(prefix-to-infix '(^ (^ (<=> p (~ q)) p ) e))     ; (((P <=> (~ Q)) ^ P) ^ E)  
(prefix-to-infix '( v (~ p) q (~ r) (~ s)))       ; ((~ P) V Q V (~ R) V (~ S))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.5
;;
;; Convierte FBF en formato infijo a FBF en formato prefijo
;;  
;; RECIBE   : FBF en formato infijo 
;; EVALUA A : FBF en formato prefijo 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun infix-to-prefix (wff)
  (when (wff-infix-p wff)         ;; Si wff no esta en formato infijo se acabo
    (if (literal-p wff)           ;; Si wff es un literal ya esta en formato prefijo e infijo
        wff
      (let ((op1 (first wff))     
            (rst_ele (rest wff))
            (conector (first (rest wff))))
        (cond
         ((unary-connector-p op1)                      ;;Si es un operado unario devolvemos la llamada recursiva con el segundo elemento de la  lista
          (list op1 (infix-to-prefix (second wff))))
         ((binary-connector-p conector)                ;; Si es un operando binario devolvamos la lista resultante de pasar op1 y op2 a formato prefijo
          (list conector (infix-to-prefix op1) (infix-to-prefix (third wff))))
         ((n-ary-connector-p conector)   ;; Si es operador unario:
          (cond
           ((null rst_ele) wff)     ;; Si es es de la forma (^) o (v) lo devolvemos.
           ((null (cdr rst_ele))    ;; Si solo tiene un elemento, es decir (op1 conector) devolvemos pasando a prefijo el op1
            (infix-to-prefix op1))
           (t (cons conector (mapcar #'(lambda (x) (infix-to-prefix x)) (list-def wff conector) ))))) ;;En otro caso devolvemos cada elemento en formato prefijo
         (t nil))))))                                                                                 ;; añadiendo el concetor al principio
          
(defun list-def (wff conector) ;; Funcion que elimina los conectores de una lista
  (remove-if #'(lambda (x) (equal conector x)) wff))
 
            
;;
;; EJEMPLOS
;;
(infix-to-prefix nil)      ;; NIL
(infix-to-prefix 'a)       ;; a
(infix-to-prefix '((a)))   ;; NIL
(infix-to-prefix '(a))     ;; NIL
(infix-to-prefix '(((a)))) ;; NIL
(prefix-to-infix (infix-to-prefix '((p v (a => (b ^ (~ c) ^ d))) ^ ((p <=> (~ q)) ^ p) ^ e)) ) 
;;-> ((P V (A => (B ^ (~ C) ^ D))) ^ ((P <=> (~ Q)) ^ P) ^ E)


(infix-to-prefix '((p v (a => (b ^ (~ c) ^ d))) ^  ((p <=> (~ q)) ^ p) ^ e))  
;; (^ (V P (=> A (^ B (~ C) D))) (^ (<=> P (~ Q)) P) E)

(infix-to-prefix '(~ ((~ p) v q v (~ r) v (~ s))))
;; (~ (V (~ P) Q (~ R) (~ S)))


(infix-to-prefix
 (prefix-to-infix
  '(V (~ P) Q (~ R) (~ S))))
;;-> (V (~ P) Q (~ R) (~ S))

(infix-to-prefix
 (prefix-to-infix
  '(~ (V (~ P) Q (~ R) (~ S)))))
;;-> (~ (V (~ P) Q (~ R) (~ S)))


(infix-to-prefix 'a)  ; A
(infix-to-prefix '((p v (a => (b ^ (~ c) ^ d))) ^  ((p <=> (~ q)) ^ p) ^ e))  
;; (^ (V P (=> A (^ B (~ C) D))) (^ (<=> P (~ Q)) P) E)

(infix-to-prefix '(~ ((~ p) v q v (~ r) v (~ s))))
;; (~ (V (~ P) Q (~ R) (~ S)))

(infix-to-prefix  (prefix-to-infix '(^ (v p (=> a (^ b (~ c) d)))))) ; '(v p (=> a (^ b (~ c) d))))
(infix-to-prefix  (prefix-to-infix '(^ (^ (<=> p (~ q)) p ) e))) ; '(^ (^ (<=> p (~ q)) p ) e))  
(infix-to-prefix (prefix-to-infix '( v (~ p) q (~ r) (~ s))))  ; '( v (~ p) q (~ r) (~ s)))
;;;

(infix-to-prefix '(p v (a => (b ^ (~ c) ^ d)))) ; (V P (=> A (^ B (~ C) D)))
(infix-to-prefix '(((P <=> (~ Q)) ^ P) ^ E))  ; (^ (^ (<=> P (~ Q)) P) E)
(infix-to-prefix '((~ P) V Q V (~ R) V (~ S))); (V (~ P) Q (~ R) (~ S))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.6
;; Predicado para determinar si una FBF es una clausula  
;;
;; RECIBE   : FBF en formato prefijo 
;; EVALUA A : T si FBF es una clausula, NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun clause-p-aux (wff)
  (cond ((null wff) t)  ;; Si es nil --> nil
        ((equal nil (literal-p (first wff))) nil) ;;Si el primero elemento no es un literal --> nil
        (t (clause-p-aux (cddr wff))))) ;; Llamamos a la funcion recursiva con el resto del resto

(defun clause-p (wff)
  (cond
   ((null wff) nil)      ;;Si wff nil --> nil
   ((not (listp wff)) nil)  ;;Si wff no es una lista --> nil
   ((equal (first wff) 'v) (clause-p-aux (rest wff))) ;;Llamamos a la funcion auxiliar
   (t nil))) ;;Cualquier otro caso nil

;;
;; EJEMPLOS:
;;
(clause-p '(v))             ; T
(clause-p '(v p))           ; T
(clause-p '(v (~ r)))       ; T
(clause-p '(v p q (~ r) s)) ; T
(clause-p NIL)                    ; NIL
(clause-p 'p)                     ; NIL
(clause-p '(~ p))                 ; NIL
(clause-p NIL)                    ; NIL
(clause-p '(p))                   ; NIL
(clause-p '((~ p)))               ; NIL
(clause-p '(^ a b q (~ r) s))     ; NIL
(clause-p '(v (^ a b) q (~ r) s)) ; NIL
(clause-p '(~ (v p q)))           ; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.7
;; Predicado para determinar si una FBF esta en FNC  
;;
;; RECIBE   : FFB en formato prefijo 
;; EVALUA A : T si FBF esta en FNC con conectores, 
;;            NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cnf-p-aux (wff)
  (cond ((null wff) t)  ;; Si es nil --> nil
        ((equal nil (clause-p (first wff))) nil) ;;Si el primero elemento no es una clausula --> nil
        (t (cnf-p-aux (rest wff))))) ;; Llamamos a la funcion recursiva con el resto del resto

(defun cnf-p (wff)
  (cond
   ((null wff) nil)      ;;Si wff nil --> nil
   ((not (listp wff)) nil)  ;;Si wff no es una lista --> nil
   ((equal (first wff) '^) (cnf-p-aux (rest wff))) ;;Llamamos a la funcion auxiliar
   (t nil))) ;;Cualquier otro caso nil


;;
;; EJEMPLOS:
;;
(cnf-p '(^ (v a  b c) (v q r) (v (~ r) s) (v a b))) ; T
(cnf-p '(^ (v a  b (~ c)) ))                        ; T
(cnf-p '(^ ))                                       ; T
(cnf-p '(^(v )))                                    ; T
(cnf-p '(~ p))                                      ; NIL
(cnf-p '(^ a b q (~ r) s))                          ; NIL
(cnf-p '(^ (v a b) q (v (~ r) s) a b))              ; NIL
(cnf-p '(v p q (~ r) s))                            ; NIL
(cnf-p '(^ (v a b) q (v (~ r) s) a b))              ; NIL
(cnf-p '(^ p))                                      ; NIL
(cnf-p '(v ))                                       ; NIL
(cnf-p NIL)                                         ; NIL
(cnf-p '((~ p)))                                    ; NIL
(cnf-p '(p))                                        ; NIL
(cnf-p '(^ (p)))                                    ; NIL
(cnf-p '((p)))                                      ; NIL
(cnf-p '(^ a b q (r) s))                            ; NIL
(cnf-p '(^ (v a  (v b c)) (v q r) (v (~ r) s) a b)) ; NIL
(cnf-p '(^ (v a (^ b c)) (^ q r) (v (~ r) s) a b))  ; NIL
(cnf-p '(~ (v p q)))                                ; NIL
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

(defun eliminate-biconditional (wff)                              ;; Transforma la bicondicional a su forma equivalente con condicional
  (if (or (null wff) (literal-p wff))                             ;; Si es nula o literal
      wff                                                         ;; devuelve el literal
    (let ((connector (first wff)))                                ;; Llama connector al primer elemento de wff
      (if (eq connector +bicond+)                                 ;; Si es efectivamente una bicondicional
          (let ((wff1 (eliminate-biconditional (second wff)))     ;; llama wff1 al elemento izquierdo de la bicondicion
                (wff2 (eliminate-biconditional (third  wff))))    ;; llama wff2 al elemento derecho de la bicondicion
            (list +and+                                           ;; Creamos el and externo entre las dos wff resultantes segun la formula
                  (list +cond+ wff1 wff2)                         ;; Creamos la condicional de wff1 a wff2 interna segun la formula
                  (list +cond+ wff2 wff1)))                       ;; Creamos la condicional de wff2 a wff1 interna segun la formula
        (cons connector                                           ;; Creamos los pares entre el conector y el mapcar correspondiente
              (mapcar #'eliminate-biconditional (rest wff)))))))  ;; Llama recursivamente a la bicondicional con el resto de elementos

;;
;; EJEMPLOS:
;;
(eliminate-biconditional '(<=> p  (v q s p) ))
;;   (^ (=> P (v Q S P)) (=> (v Q S P) P))
(eliminate-biconditional '(<=>  (<=> p  q) (^ s (~ q))))
;;   (^ (=> (^ (=> P Q) (=> Q P)) (^ S (~ Q)))
;;      (=> (^ S (~ Q)) (^ (=> P Q) (=> Q P))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.2
;; Dada una FBF, que contiene conectores => evalua a
;; una FBF equivalente que no contiene el connector =>
;;
;; RECIBE   : wff en formato prefijo sin el connector <=> 
;; EVALUA A : wff equivalente en formato prefijo 
;;            sin el connector =>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eliminate-conditional (wff)                                ;; Transforma la condicional a su forma equivalente segun la regla
  (if (or (null wff) (literal-p wff))                             ;; Si es nula o literal
      wff                                                         ;; devuelve el literal
    (let ((connector (first wff)))                                ;; Llama connector al primer elemento de wff
      (if (eq connector +cond+)                                   ;; Si es efectivamente una condicional
          (let ((wff1 (eliminate-conditional (second wff)))       ;; Llama wf1 al elemento izquierdo de la condicional
                (wff2 (eliminate-conditional (third  wff))))      ;; Llama wf2 al elemento derecho de la condicional
            (list +or+                                            ;; Creamos el or externo entre las dos wff resultantes segun la formula
                  (list +not+ wff1)                               ;; Negamos la wff1 interna segun la formula
                  wff2))                                          ;; Dejamos la otra wff2 interna como lista
        (cons connector                                           ;; Creamos los pares entre el conector y el mapcar correspondiente
              (mapcar #'eliminate-conditional (rest wff)))))))    ;; Llama recursivamente a la condicional con el resto de elementos

;;
;; EJEMPLOS:
;;
(eliminate-conditional '(=> p q))                      ;;; (V (~ P) Q)
(eliminate-conditional '(=> p (v q s p)))              ;;; (V (~ P) (V Q S P))
(eliminate-conditional '(=> (=> (~ p) q) (^ s (~ q)))) ;;; (V (~ (V (~ (~ P)) Q)) (^ S (~ Q)))

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
(defun exchange-and-or (connector)                       ;; Funcion Auxiliar que intercambia el conector entre and y or
  (cond                                                  
   ((eq connector +and+) +or+)                           ;; Si el conector es un and, devuelvo el or
   ((eq connector +or+) +and+)                           ;; Si el conector es un or, devuelvo el and
   (t connector)))                                       ;; Si no devolvemos true

(defun reduce-scope-of-negation (wff)                                       ;; Funcion principal que reduce al ambito de la negacion
  (if (or (null wff) (literal-p wff))                                       ;; Caso base, si la wff es nula o literal
    wff                                                                     ;; devuelve la propia wff
      (if (eq +not+ (first wff))                                            ;; si el primer elemento es not
        (if (negative-literal-p (second wff))                               ;; y el segundo es un literal negativo
          (second (second wff))                                             ;; aplicamos (notnot a) = a
          (if (eq +not+ (first (second wff)))                               ;; si no, miramos el primer elemento de la expresion interna
            (reduce-scope-of-negation (rest (second wff)))                  ;; si es un not llamamos recursivamente sobre el
            (cons (exchange-and-or (first (second wff)))                    ;; si no es un not, es un and o un or  y aplicamos demorgan
              (mapcar #'(lambda(x)                                          ;; para aplicar demorgan hacemos un mapcar de lo interno
                (reduce-scope-of-negation (list +not+ x))) (rest (second wff))))))   ;; y usamos la formula
        (cons (first wff)
          (mapcar #'reduce-scope-of-negation (rest wff))))))                ;; caso de que no sea un not lo primero, llamamos recursivamente

;;
;;  EJEMPLOS:
;;
(reduce-scope-of-negation '(~ (v p (~ q) r))) 
;;; (^ (~ P) Q (~ R))
(reduce-scope-of-negation '(~ (^ p (~ q) (v  r s (~ a))))) 
;;;  (V (~ P) Q (^ (~ R) (~ S) A))

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
(defun combine-elt-lst (elt lst)                              ;; Le pasamos un elemento y una lista y los combina
  (if (null lst)                                              ;; Si la lista es nula
      (list (list elt))                                       ;; devuelve ((elt))
    (mapcar #'(lambda (x) (cons elt x)) lst)))                ;; si no, devuelve las combinaciones del elt con la lista

(defun exchange-NF (nf)                                       ;; Dada una forma normal, la intercambia
  (if (or (null nf) (literal-p nf))                           ;; si la forma normal es nula o literal
      nf                                                      ;; la devuelve tal cual
    (let ((connector (first nf)))                             ;; llamamos conector al primer elemento de la forma normal
      (cons (exchange-and-or connector)                       ;; hacemos una lista intercambiando el conector como primer elemento
            (mapcar #'(lambda (x)                             ;; como segundo elemento, hacemos un mapcar
                          (cons connector x))                 ;; de la sublista formada por el conector y el resultado de
                (exchange-NF-aux (rest nf)))))))              ;; llamar a intercambiar NF aux con toda la nf menos el conector primero

(defun exchange-NF-aux (nf)                                        ;; Funcion auxiliar del intercambio de una forma normal 
  (if (null nf)                                                    ;; si la nf es nula 
      NIL                                                          ;; devuelve nil
    (let ((lst (first nf)))                                        ;; llamamos lst al primer elemento de la nf
      (mapcan #'(lambda (x)                                        ;; realizamos un mapcar eliminando los nil
                  (combine-elt-lst  
                   x                                               ;; combina el elt resultante del if de abajo
                   (exchange-NF-aux (rest nf))))                   ;; con el resto de la nf
        (if (literal-p lst) (list lst) (rest lst))))))             ;; si lst es un literal, lo hacemos lista y si no, pasamos el resto de la lst

(defun simplify (connector lst-wffs )                              ;; Simplifica con un conector, unas listas de fbf prefijo
  (if (literal-p lst-wffs)                                         ;; si son un literal
      lst-wffs                                                     ;; devuelve la lista de fbf prefijo tal cual                    
    (mapcan #'(lambda (x)                                          ;; hacemos un mapcar eliminando nils
                (cond 
                 ((literal-p x) (list x))                          ;; si alguna sublista es literal, la hace lista y la devuelve
                 ((equal connector (first x))                      ;; si el primer elemento de la sublista es el conector a simplificar
                  (mapcan                                          ;; hacemos un mapcar eliminando nils
                      #'(lambda (y) (simplify connector (list y))) ;; Llamamos recursivamente a simplificar con el resto de sublistas
                    (rest x))) 
                 (t (list x))))                                    ;; Si no entra en ninguna de las dos condiciones anteriores, devuelve la sublista               
      lst-wffs)))

(defun cnf (wff)                                                          ;; Traduce una FBF prefijo en FNC apoyandose en las funciones auxiliares anteriores
  (cond
   ((cnf-p wff) wff)                                                      ;; Si la FBF está ya en FNC la devuelve
   ((literal-p wff)                                                       ;; Si la FBF es un literal
    (list +and+ (list +or+ wff)))                                         ;; Devuelve (v (^ wff))
   ((let ((connector (first wff)))                                        ;; Llamamos conector al primer elemento de la wff 
      (cond
       ((equal +and+ connector)                                           ;; si el conector es un v 
        (cons +and+ (simplify +and+ (mapcar #'cnf (rest wff)))))          ;; devuelve una lista simplificada con ands 
       ((equal +or+ connector)                                            ;; si el conector es un ^
        (cnf (exchange-NF (cons +or+ (simplify +or+ (rest wff)))))))))))  ;; devuelve una lista simplificada con ors


(cnf 'a)

(cnf '(v (~ a) b c))
(print (cnf '(^ (v (~ a) b c) (~ e) (^ e f (~ g) h) (v m n) (^ r s q) (v u q) (^ x y))))
(print (cnf '(v (^ (~ a) b c) (~ e) (^ e f (~ g) h) (v m n) (^ r s q) (v u q) (^ x y))))
(print (cnf '(^ (v p  (~ q)) a (v k  r  (^ m  n)))))
(print (cnf '(v p  q  (^ r  m)  (^ n  a)  s )))
(exchange-NF '(v p  q  (^ r  m)  (^ n  a)  s ))
(cnf '(^ (v a b (^ y r s) (v k l)) c (~ d) (^ e f (v h i) (^ o p))))
(cnf '(^ (v a b (^ y r s)) c (~ d) (^ e f (v h i) (^ o p))))
(cnf '(^ (^ y r s (^ p q (v c d))) (v a b)))
(print (cnf '(^ (v (~ a) b c) (~ e) r s 
                (v e f (~ g) h) k (v m n) d)))
;;
(cnf '(^ (v p (~ q)) (v k r (^ m  n))))
(print  (cnf '(v (v p q) e f (^ r  m) n (^ a (~ b) c) (^ d s))))
(print (cnf '(^ (^ (~ y) (v r (^ s (~ x)) (^ (~ p) m (v c d))) (v (~ a) (~ b))) g)))
;;
;; EJEMPLOS:
;;
(cnf NIL)              ; NIL
(cnf 'a)               ; (^ (V A))
(cnf '(~ a))           ; (^ (V (~ A)))
(cnf '(V (~ P) (~ P))) ; (^ (V (~ P) (~ P)))
(cnf '(V A))           ; (^ (V A))
(cnf '(^ (v p (~ q)) (v k r (^ m  n))))
;;;   (^ (V P (~ Q)) (V K R M) (V K R N))
(print  (cnf '(v (v p q) e f (^ r  m) n (^ a (~ b) c) (^ d s))))
;;; (^ (V P Q E F R N A D)      (V P Q E F R N A S)
;;;    (V P Q E F R N (~ B) D)  (V P Q E F R N (~ B) S)
;;;    (V P Q E F R N C D)      (V P Q E F R N C S) 
;;;    (V P Q E F M N A D)      (V P Q E F M N A S) 
;;;    (V P Q E F M N (~ B) D)  (V P Q E F M N (~ B) S) 
;;;    (V P Q E F M N C D)      (V P Q E F M N C S))
;;;
(print 
 (cnf '(^ (^ (~ y) (v r (^ s (~ x)) 
                      (^ (~ p) m (v c d)))(v (~ a) (~ b))) g)))
;;;(^ (V (~ Y)) (V R S (~ P)) (V R S M) 
;;;   (V R S C D) (V R (~ X) (~ P)) 
;;;   (V R (~ X) M) (V R (~ X) C D)
;;;   (V (~ A) (~ B)) (V G))  

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

(defun eliminate-connectors (cnf)
  (mapcar #'rest (rest cnf)))       ;; El mapcar elimina el conector, 
                                    ;; que sabemos que será el primer elemento de las cnfs 

(eliminate-connectors 'nil)
(eliminate-connectors (cnf '(^ (v p  (~ q))  (v k  r  (^ m  n)))))
(eliminate-connectors
 (cnf '(^ (v (~ a) b c) (~ e) (^ e f (~ g) h) (v m n) (^ r s q) (v u q) (^ x y))))

(eliminate-connectors (cnf '(v p  q  (^ r  m)  (^ n  q)  s )))
(eliminate-connectors (print (cnf '(^ (v p  (~ q)) (~ a) (v k  r  (^ m  n))))))

(eliminate-connectors '(^))
(eliminate-connectors '(v))
(eliminate-connectors '(^ (v p (~ q)) (v) (v k r)))
(eliminate-connectors '(^ (v a b)))

;;   EJEMPLOS:
;;

(eliminate-connectors '(^ (v p (~ q)) (v k r)))
;; ((P (~ Q)) (K R))
(eliminate-connectors '(^ (v p (~ q)) (v q (~ a)) (v s e f) (v b)))
;; ((P (~ Q)) (Q (~ A)) (S E F) (B))


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

;; Para los comentarios de la funcion, leerlos de abajo arriba para seguir la logica.

(defun wff-infix-to-cnf (wff)
  (if (or (null wff) (literal-p wff))        ;; Casos bases de nula o literal
    wff
    (eliminate-connectors                    ;; Eliminamos los conectores
      (cnf                                   ;; La transformamos a forma normal conjuntiva
        (reduce-scope-of-negation            ;; Reducimos al ambito de la negacion
          (eliminate-conditional             ;; Hacemos lo propio con la condicional
            (eliminate-biconditional         ;; Eliminamos la bicondicional del prefijo
              (infix-to-prefix wff))))))))   ;; Lo primero, pasamos la forma a prefijo

;;
;; EJEMPLOS:
;; 
(wff-infix-to-cnf 'a)
(wff-infix-to-cnf '(a))
(wff-infix-to-cnf  '( (~ p) v q v (~ r) v (~ s)))
(wff-infix-to-cnf  '((p v (a => (b ^ (~ c) ^ d))) ^ ((p <=> (~ q)) ^ p) ^ e))
;; ((P (~ A) B) (P (~ A) (~ C)) (P (~ A) D) ((~ P) (~ Q)) (Q P) (P) (E))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.1
;; eliminacion de literales repetidos una clausula 
;; 
;; RECIBE   : K - clausula (lista de literales, disyuncion implicita)
;; EVALUA A : clausula equivalente sin literales repetidos 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eliminate-repeated-literals-aux (k)
  (cond 
   ((equal k nil ) '(nil))
   ((equal (find (first k) (rest k) :test #'equal) nil) (cons (first k) (eliminate-repeated-literals (rest k))))
   (t (eliminate-repeated-literals (rest k)))))

(defun eliminate-repeated-literals (k)
  (if (null k)
      nil
    (eliminate-repeated-literals-aux (remove nil k :test #'equal))))

(eliminate-repeated-literals'(nil nil nil))

;;
;; EJEMPLO:
;;
(eliminate-repeated-literals '(a b (~ c) (~ a) a c (~ c) c a))
;;;   (B (~ A) (~ C) C A)


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
   ((equal cnf nil) nil)  ;; Si cnf nil --> nil
   ((equal (find operador (rest cnf) :test #'our-equal) nil) (cons operador (eliminate-repeated-clauses (rest cnf)))) ;; El test es con la igualdad de conjuntos definida
   (t (eliminate-repeated-clauses (rest cnf)))))) ;; LLamada recusriva con el resto de la lista si no hay ninguna clausula igual

;; Funcion que nos determina si L1 esta contenida en L2 (contenido de conjuntos)
(defun contenido (l1 l2)  
  (if (equal nil l1)   ;; Nil simepre esta contenido en otra lista
      t
    (and (not (equal nil (find (first l1) (eliminate-repeated-literals l2) :test #'equal))) ;; Utilizamos el test equal para que tome el igual por valor
         (contenido (rest l1) l2))))

;; Definimos que dos clausulas si l1 esta contenido en l2 y viceversa
(defun our-equal (l1 l2)
  (and (contenido l1 l2) (contenido l2 l1)))

;;
;; EJEMPLO:
;;
(eliminate-repeated-clauses '(((~ a) c) (c (~ a)) ((~ a) (~ a) b c b) (a a b) (c (~ a) b  b) (a b)))
;;; ((C (~ A)) (C (~ A) B) (A B))

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
      (list k1)
    nil))
  
;;
;;  EJEMPLOS:
;;
(subsume '(a) '(a b (~ c)))
;; ((a))
(subsume NIL '(a b (~ c)))
;; (NIL)
(subsume '(a b (~ c)) '(a) )
;; NIL
(subsume '( b (~ c)) '(a b (~ c)) )
;; (( b (~ c)))
(subsume '(a b (~ c)) '( b (~ c)))
;; NIL
(subsume '(a b (~ c)) '(d  b (~ c)))
;; nil
(subsume '(a b (~ c)) '((~ a) b (~ c) a))
;; ((A B (~ C)))
(subsume '((~ a) b (~ c) a) '(a b (~ c)) )
;; nil

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.4
;; eliminacion de clausulas subsumidas en una FNC 
;; 
;; RECIBE   : K (clausula), cnf (FBF en FNC)
;; EVALUA A : FBF en FNC equivalente a cnf sin clausulas subsumidas 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eliminate-subsumed-clauses (cnf)
  (remove-subsumed cnf cnf))

;; Función que determina si k subsume en algina clausula de l
(defun is-subsumed (k l)
  (unless (null l)
    (or (and (not(null (subsume (car l) k)))   ;;si k subsume a una cláusula de l y no es él mismo hacemos que devuelva true
             (not (eq k (car l))))
        (is-subsumed k (rest l)))))            ;;Llamada recursiva la función sobre la lista

(is-subsumed '(a b c) '( (a b) (c d)))
(is-subsumed '(a b c) '((a b c) '(a b c)))

;; Funcion que nos va a servir para eliminar clausulas subsumidas. Trabaja con dos listas que al comienzo son la misma
(defun remove-subsumed (l1 final)
  (cond
   ((null l1) nil) ;;Si es nil l1 y por tanto nil final --> nil
   ((is-subsumed (first l1) final) (remove-subsumed (rest l1) final)) ;; Si el primer elemento subsume alguno de la lista original, llamda recursiva con el resto 
                                                                      ;; de la lista para eliminarlo
   (t (cons (first l1) (remove-subsumed (rest l1) final )))))         ;; Si no subsume metemos el elemento al resultado de la llamda recursiva sobre el resto

 
;;
;;  EJEMPLOS:
;;
(eliminate-subsumed-clauses 
 '((a b c) (b c) (a (~ c) b)  ((~ a) b) (a b (~ a)) (c b a)))
;;; ((A (~ C) B) ((~ A) B) (B C)) ;; el orden no es importante
(eliminate-subsumed-clauses
 '((a b c) (b c) (a (~ c) b) (b)  ((~ a) b) (a b (~ a)) (c b a)))
;;; ((B))
(eliminate-subsumed-clauses
 '((a b c) (b c) (a (~ c) b) ((~ a))  ((~ a) b) (a b (~ a)) (c b a)))
;;; ((A (~ C) B) ((~ A)) (B C))

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
    (if (equal k nil)    ;; Si k nil --> nil
        nil
      (or (tautology-p (rest k)) (en-tauto op k))))) ;; Comprobamos la condicion de en-tauto para todos los literales de K.

;; Definimos una igualdad de literales un tanto especial que nos va a venri muy bien para deteminar tautologias
(defun equal-inv (a b)
  (cond
   ((and (positive-literal-p a) (negative-literal-p b))   ;; Si a = p y  b = (~ c). Comprobamos si P es igual a c
    (equal (second b) a))
  ((and (negative-literal-p a) (positive-literal-p b))    ;; Caso contrario al anterior.
    (equal (second a) b))
   (t nil)))
    
(equal-inv 'a '(¬ a))

(defun en-tauto (a k)  ;; Función que busca en función de la igualdad definida si un literal es "igual" a otro de k.
  (if (equal (find a k :test #'equal-inv) nil)
      nil
    t))

(en-tauto 'b '(a a b (~ b) c))

;;
;;  EJEMPLOS:
;;
(tautology-p '(B A))
(tautology-p '((~ B) A C (~ A) D)) ;;; T 
(tautology-p '((~ B) A C D))       ;;; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.6
;; eliminacion de clausulas en una FBF en FNC que son tautologia
;;
;; RECIBE   : cnf - FBF en FNC
;; EVALUA A : FBF en FNC equivalente a cnf sin tautologias 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eliminate-tautologies (cnf) 
  (cond
   ((null cnf) nil)   ;; Caso base nil --> nil
   ((tautology-p (first cnf)) (eliminate-tautologies (rest cnf)))  ;; Si el primero es tautologia lo eliminamos
   (t (cons (first cnf) (eliminate-tautologies(rest cnf)))))) ;; Si no lo concatenamos a lo que de vuleve la funcón con el resto

;;
;;  EJEMPLOS:
;;
(eliminate-tautologies 
 '(((~ b) a) (a (~ a) b c) ( a (~ b)) (s d (~ s) (~ s)) (a)))
;; (((~ B) A) (A (~ B)) (A))

(eliminate-tautologies '((a (~ a) b c)))
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
  (eliminate-subsumed-clauses(eliminate-tautologies(eliminate-repeated-clauses cnf)))) ;;Llamamos al resto de funciones para que simplifican la expresion

;;
;;  EJEMPLOS:
;;
(simplify-cnf '((a a) (b) (a) ((~ b)) ((~ b)) (a b c a)  (s s d) (b b c a b)))
;; ((B) ((~ B)) (S D) (A)) ;; en cualquier orden

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.1
;; Construye el conjunto de clausulas lambda-neutras para una FNC 
;;
;; RECIBE   : cnf    - FBF en FBF simplificada
;;            lamda - literal positivo
;; EVALUA A : cnf_lamda^(0) subconjunto de clausulas de cnf  
;;            que no contienen el literal lambda ni Â¬lambda   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Funcion principal que llama a la interseccion de las dos funciones auxiliares
(defun extract-neutral-clauses (lamda cnf) 
  (intersection (extract-no-positive-clauses lamda cnf) (extract-no-negative-clauses lamda cnf)))

;; Funcion auxiliar que devuelve aquellas clausulas que no tienen el literal positivo lamda
(defun extract-no-positive-clauses (lamda cnf) 
  (if (or (null cnf) (literal-p cnf))                                     ;; Manejo caso base
    NIL
    (if (equal (member lamda (first cnf) :test #'equal) NIL)              ;; Si lamda no es miembro
      (cons (first cnf) (extract-no-positive-clauses lamda (rest cnf)))   ;; Concatena la clausula y llama recursivamente con el resto de clausulas
      (extract-no-positive-clauses lamda (rest cnf)))))                   ;; Si es miembro, entonces no lo devolvemos, solo llamamos recursivamente

;; Funcion auxiliar que devuelve aquellas clausulas que no tienen el literal negativo de lamda
(defun extract-no-negative-clauses (lamda cnf)                            ;; Manejo caso base
  (if (or (null cnf) (literal-p cnf))
    NIL
    (if (equal (member (list +not+ lamda) (first cnf) :test #'equal) NIL) ;; Si no lamda no es miembro
      (cons (first cnf) (extract-no-negative-clauses lamda (rest cnf)))   ;; Concatena la clausula y llama recursivamente con el resto de clausulas
      (extract-no-negative-clauses lamda (rest cnf)))))                   ;; Si es miembro, entonces no lo devolvemos, solo llamamos recursivamente

;;
;;  EJEMPLOS:
;;
(extract-neutral-clauses 'p
                           '((p (~ q) r) (p q) (r (~ s) q) (a b p) (a (~ p) c) ((~ r) s)))
;; ((R (~ S) Q) ((~ R) S))


(extract-neutral-clauses 'r NIL)
;; NIL

(extract-neutral-clauses 'r '(NIL))
;; (NIL)

(extract-neutral-clauses 'r
                           '((p (~ q) r) (p q) (r (~ s) q) (a b p) (a (~ p) c) ((~ r) s)))
;; ((P Q) (A B P) (A (~ P) C))

(extract-neutral-clauses 'p
                           '((p (~ q) r) (p q) (r (~ s) p q) (a b p) (a (~ p) c) ((~ r) p s)))
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
  (if (or (null cnf) (literal-p cnf))                                   ;; Manejo caso base
    NIL
    (if (equal (member lamda (first cnf) :test #'equal) NIL)            ;; Si lamda no es miembro
      (extract-positive-clauses lamda (rest cnf))                       ;; Entonces no lo devolvemos, solo llamamos recursivamente
      (cons (first cnf) (extract-positive-clauses lamda (rest cnf)))))) ;; Si no, concatena la clausula y llama recursivamente con el resto de clausulas


;;
;;  EJEMPLOS:
;;
(extract-positive-clauses 'p
                             '((p (~ q) r) (p q) (r (~ s) q) (a b p) (a (~ p) c) ((~ r) s)))

;; ((P (~ Q) R) (P Q) (A B P))


(extract-positive-clauses 'r NIL)
;; NIL
(extract-positive-clauses 'r '(NIL))
;; NIL
(extract-positive-clauses 'r
                             '((p (~ q) r) (p q) (r (~ s) q) (a b p) (a (~ p) c) ((~ r) s)))
;; ((P (~ Q) R) (R (~ S) Q))
(extract-positive-clauses 'p
                             '(((~ p) (~ q) r) ((~ p) q) (r (~ s) (~ p) q) (a b (~ p)) ((~ r) (~ p) s)))
;; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.3
;; Construye el conjunto de clausulas lambda-negativas para una FNC 
;;
;; RECIBE   : cnf    - FBF en FNC simplificada
;;            lambda - literal positivo 
;; EVALUA A : cnf_lambda^(-) subconjunto de clausulas de cnf  
;;            que contienen el literal lambda  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun extract-negative-clauses (lamda cnf) 
  (if (or (null cnf) (literal-p cnf))                                     ;; Manejo caso base
    NIL
    (if (equal (member (list +not+ lamda) (first cnf) :test #'equal) NIL) ;; Si no lamda no es miembro
      (extract-negative-clauses lamda (rest cnf))                         ;; Entonces no lo devolvemos, solo llamamos recursivamente
      (cons (first cnf) (extract-negative-clauses lamda (rest cnf))))))   ;; Si no, concatena la clausula y llama recursivamente con el resto de clausulas

;;
;;  EJEMPLOS:
;;
(extract-negative-clauses 'p
                             '((p (~ q) r) (p q) (r (~ s) q) (a b p) (a (~ p) c) ((~ r) s)))
;; ((A (~ P) C))

(extract-negative-clauses 'r NIL)
;; NIL
(extract-negative-clauses 'r '(NIL))
;; NIL
(extract-negative-clauses 'r
                             '((p (~ q) r) (p q) (r (~ s) q) (a b p) (a (~ p) c) ((~ r) s)))
;; (((~ R) S))
(extract-negative-clauses 'p
                             '(( p (~ q) r) ( p q) (r (~ s) p q) (a b p) ((~ r) p s)))
;; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.4
;; resolvente de dos clausulas
;;
;; RECIBE   : lamda      - literal positivo
;;            K1, K2      - clausulas simplificadas
;; EVALUA A : res_lamda(K1,K2) 
;;                        - lista que contiene la 
;;                          clausula que resulta de aplicar resolucion 
;;                          sobre K1 y K2, con los literales repetidos 
;;                          eliminados
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun resolve-on (lamda K1 K2)
  (if (or (equal K1 nil) (equal K2 nil))                                              ;; Manejo del caso base
    NIL
    (cond 
      ((and (not (equal (member lamda K1 :test #'equal) nil))                         ;; Si lamda es miembro de K1
        (not (equal (member (list +not+ lamda) K2 :test #'equal) nil)))               ;; y no lamda es miembro de K2
          (list(eliminate-repeated-literals (union (remove lamda K1 :test #'equal)    ;; devuelve las clausulas
            (remove (list +not+ lamda) K2 :test #'equal)))))                          ;; resultantes tras eliminar lamda
      ((and (not (equal (member (list +not+ lamda) K1 :test #'equal) nil))            ;; Caso contrario
        (not (equal (member lamda K2 :test #'equal) nil)))                            
          (list(eliminate-repeated-literals (union (remove (list +not+ lamda) K1 :test #'equal) 
            (remove lamda K2 :test #'equal)))))
      (t  nil))))                                                                     ;; En cualquier otro caso, no resuelvo
;;
;;  EJEMPLOS:
;;
(resolve-on 'p '(a b (~ c) p) '((~ p) b a q r s))
;; (((~ C) B A Q R S))

(resolve-on 'p '(a b (~ c) (~ p)) '( p b a q r s))
;; (((~ C) B A Q R S))

(resolve-on 'p '(p) '((~ p)))
;; (NIL)


(resolve-on 'p NIL '(p b a q r s))
;; NIL

(resolve-on 'p NIL NIL)
;; NIL

(resolve-on 'p '(a b (~ c) (~ p)) '(p b a q r s))
;; (((~ C) B A Q R S))

(resolve-on 'p '(a b (~ c)) '(p b a q r s))
;; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.5
;; Construye el conjunto de clausulas RES para una FNC 
;;
;; RECIBE   : lamda - literal positivo
;;            cnf    - FBF en FNC simplificada
;;            
;; EVALUA A : RES_lambda(cnf) con las clauses repetidas eliminadas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;; Funcion auxiliar que resuelve para cada lamda
(defun aux (lamda clause cnf)
  (mapcan #'(lambda(x) (if (not(equal x lamda)) (resolve-on lamda clause x) nil)) cnf))

;; Funcion principal que llama recursivamente a la auxiliar para todos los lamdas
(defun build-RES (lamda cnf)
  (eliminate-repeated-literals(eliminate-repeated-clauses (union (extract-neutral-clauses lamda cnf) (mapcan #'(lambda (x) (aux lamda x cnf)) (eliminate-repeated-clauses cnf))))))

;;
;;  EJEMPLOS:
;;
(build-RES 'p NIL)
;; NIL
(build-RES 'P '((A  (~ P) B) (A P) (A B)));; ((A B))
(build-RES 'P '((B  (~ P) A) (A P) (A B)));; ((B A))

(build-RES 'p '(NIL))
;; (NIL)

(build-RES 'p '((p) ((~ p))))
;; (NIL)

(build-RES 'q '((p q) ((~ p) q) (a b q) (p (~ q)) ((~ p) (~ q))))
;; ((P) ((~ P) P) ((~ P)) (B A P) (B A (~ P)))

(build-RES 'p '((p q) (c q) (a b q) (p (~ q)) (p (~ q))))
;; ((A B Q) (C Q))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.5
;; Comprueba si una FNC es SAT calculando RES para todos los
;; atomos en la FNC 
;;
;; RECIBE   : cnf - FBF en FNC simplificada
;; EVALUA A : T  si cnf es SAT
;;                NIL  si cnf es UNSAT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun  RES-SAT-p (cnf) 
  (cond
    ((null cnf) t)                                            ;; caso base
    ((equal '(nil) cnf) nil)                                  ;; segundo caso base
    ((member nil cnf) nil)                                    ;; caso en el que haya algún nil
    (t (RES-SAT-p-rec (extract-positive-literals cnf) cnf))   ;; llamamos a la funcion auxiliar recursiva
    )  
  )

;; Funcion recursiva para poder implementar el algoritmo
(defun RES-SAT-p-rec (lamdas cnf)
  (cond
   ((or (null cnf) (null lamdas)) t)                ;; caso base
    (t 
      (let* ((newlamda (first lamdas))              
             (newalpha (simplify-cnf (build-RES newlamda cnf))))
        (if (some #'null newalpha)
            nil
          (RES-SAT-p-rec (rest lamdas) newalpha)))))) ;; Aplicamos el algoritmo dado, simplificar de resolver

;; Funcion auxiliar que extrae los literales positivos de una cnf a una lista
(defun extract-positive-literals (cnf)
  (if (null cnf)
    NIL                                                                ;; caso base
    (eliminate-repeated-literals (remove nil (mapcan #'(lambda (x)     
      (mapcar #'(lambda (y) 
        (if (equal (positive-literal-p y) t)
            y nil)) x))cnf)))))
;;
;;  EJEMPLOS:
;;
;;
;; SAT Examples
;;
(RES-SAT-p nil)  ;;; T
(RES-SAT-p '((p) ((~ q)))) ;;; T 
(RES-SAT-p
 '((a b d) ((~ p) q) ((~ c) a b) ((~ b) (~ p) d) (c d (~ a)))) ;;; T 
(RES-SAT-p
 '(((~ p) (~ q) (~ r)) (q r) ((~ q) p) ((~ q)) ((~ p) (~ q) r))) ;;;T
(RES-SAT-p '((P (~ Q)) (K R))) ;;; T
;;
;; UNSAT Examples
;;
(RES-SAT-p '((P (~ Q)) NIL (K R))) ;;; NIL
(RES-SAT-p '(nil))         ;;; NIL
(RES-SAT-p '((S) nil))     ;;; NIL 
(RES-SAT-p '((p) ((~ p)))) ;;; NIL
(RES-SAT-p
 '(((~ p) (~ q) (~ r)) (q r) ((~ q) p) (p) (q) ((~ r)) ((~ p) (~ q) r))) ;;; NIL
(RES-SAT-p '(((~ q)) (q) ((~ A))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.6:
;; Resolucion basada en RES-SAT-p
;;
;; RECIBE   : wff - FBF en formato infijo 
;;            w   - FBF en formato infijo 
;;                               
;; EVALUA A : T   si w es consecuencia logica de wff
;;            NIL en caso de que no sea consecuencia logica.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; funcion auxiliar que devuelve not w 
(defun notw (w)
  (if (negative-literal-p w)         ;; si es negativo w
    (list(list(second w)))           ;; devuelvo w
    (list(list(list +not+ w)))))     ;; si no, not w

(notw '(~ a))

;; Funcion principal que aplica el algoritmo
(defun logical-consequence-RES-SAT-p (wff w)
  (not (res-sat-p (union
                   (simplify-cnf (wff-infix-to-cnf wff))
                   (notw w)))))     ;; metemos w negado y hacemos resolucion con wff

;;
;;  EJEMPLOS:
;;
(wff-infix-to-cnf '(q))


(logical-consequence-RES-SAT-p NIL 'a) ;;; NIL
(logical-consequence-RES-SAT-p NIL NIL) ;;; NIL
(logical-consequence-RES-SAT-p '(q ^ (~ q)) 'a) ;;; T 


(logical-consequence-RES-SAT-p '((p => (~ p)) ^ p) 'q)
;; T

(logical-consequence-RES-SAT-p '((p => (~ p)) ^ p) '(~ q))
;; T

(logical-consequence-RES-SAT-p '((p => q) ^ p) 'q)
;; T

(logical-consequence-RES-SAT-p '((p => q) ^ p) '(~ q))
;; NIL

(logical-consequence-RES-SAT-p 
 '(((~ p) => q) ^ (p => (a v (~ b))) e^ (p => ((~ a) ^ b)) ^ ( (~ p) => (r  ^ (~ q)))) 
 '(~ a))
;; T

(logical-consequence-RES-SAT-p 
 '(((~ p) => q) ^ (p => (a v (~ b))) ^ (p => ((~ a) ^ b)) ^ ( (~ p) => (r  ^ (~ q)))) 
 'a)
;; T

(logical-consequence-RES-SAT-p 
 '(((~ p) => q) ^ (p => ((~ a) ^ b)) ^ ( (~ p) => (r  ^ (~ q)))) 
 'a)
;; NIL

(logical-consequence-RES-SAT-p 
 '(((~ p) => q) ^ (p => ((~ a) ^ b)) ^ ( (~ p) => (r  ^ (~ q)))) 
 '(~ a))
;; T

(logical-consequence-RES-SAT-p 
 '(((~ p) => q) ^ (p <=> ((~ a) ^ b)) ^ ( (~ p) => (r  ^ (~ q)))) 
 'q)
;; NIL

(logical-consequence-RES-SAT-p 
 '(((~ p) => q) ^ (p <=> ((~ a) ^ b)) ^ ( (~ p) => (r  ^ (~ q)))) 
 '(~ q))
;; NIL

(or 
 (logical-consequence-RES-SAT-p '((p => q) ^ p) '(~q))      ;; NIL
 (logical-consequence-RES-SAT-p 
  '(((~ p) => q) ^ (p => ((~ a) ^ b)) ^ ( (~ p) => (r  ^ (~ q)))) 
  'a) ;; NIL
 (logical-consequence-RES-SAT-p 
  '(((~ p) => q) ^ (p <=> ((~ a) ^ b)) ^ ( (~ p) => (r  ^ (~ q)))) 
  'q) ;; NIL
 (logical-consequence-RES-SAT-p 
  '(((~ p) => q) ^ (p <=> ((~ a) ^ b)) ^ ( (~ p) => (r  ^ (~ q)))) 
  '(~ q)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    EJERCICIO 5                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 5.3:
;; Algoritmo BFS (Breadth-first-search) en grafos
;;
;; RECIBE   : end El nodo meta
;;            queue  cola de nodos encontrados pero no visitados
;;            net el grafo (inicialmente con el nodo raíz y sus hijos
;;                   
;; EVALUA A : T   si w es consecuencia logica de wff
;;            NIL en caso de que no sea consecuencia logica.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Breadth-first-search in graphs
;;;
(defun bfs (end queue net)
  (if (null queue) ’()                                       ;; Si la cola de nodos a explorar está vacía termina el algoritmo (devolvemos lista vacía)
   (let* ((path (first queue))                               ;; path es el nodo y sus vecinos del grafo a explorar en el siguiente paso del algoritmo
          (node (first path)))                               ;; node hace refencia al nodo a explorar
     (if (eql node end)                                      ;; Si el nodo es igual a la meta hemos terminado
         (reverse path)                                      ;; Si no llamamos recursivamente al algoritmo con la misma meta (no cambia), el resto de la cola de nodos
       (bfs end                                              ;; encontrados pero no explorados (ya que el primero de esta acaba de ser explorado) y metemos en la lista
            (append (rest queue)                             ;; lo devuelto pr la funcion auxiliar. También le pasamos el grafo que no varía
                    (new-paths path node net))
            net)))))

(defun new-paths (path node net)                             ;; Funcion que devuelve el nodo hijo a explorar en la iteracion n del algoritmo junto con el camino para
  (mapcar #'(lambda(n)                                       ;; que se ha seguido para llegar hasta el.
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
	(if (null queue) '()
		(let* ((path (first queue))
			(node (first path)))
		(if (eql node end)
			(reverse path)
		  (bfs end
		  	(append (rest queue)
		  		(new-paths path node net))
		  	net)))))

(defun new-paths (path node net)
	(mapcar #'(lambda(n)
		(cons n path))
   (rest (assoc node net))))

(defun shortest-path (start end net) (bfs end (list (list start)) net))


(new-paths '(a d) 'a '((a d) (b d f) (c e) (d f) (e b f) (f)))
(bfs 'f '((c)) '((a d) (b d f) (c e) (d f) (e b f) (f)))

 ;;Encuentra el camino más corto, ya que vamos apliando el camino desde el nodo inicial en
(shortest-path 'c 'f '((a d) (b d f) (c e) (d f) (e b f) (f)))          ;; todas las direcciones hasta encontrar por primera vez el nodo final




(shortest-path 'a 'f '((a d) (b d f) (c e) (d f) (e b f) (f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 5.8:
;; Algoritmo BFS-improved
;;
;; RECIBE   : end 
;;            queue   - F 
;;            net - F
;;                   
;; El problema es que se vuelven a explorar nodos ya explorados
;; Sol: eliminación de los estados repetidos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nodo-in-path (l) ;; Funcion que sirve para ver si un elemento esta en el path
  (or (null l)
      (and (not (member (first l) (rest l)))
           (nodo-in-path (rest l)))))
      

(defun new-paths-improved (path node net)   ;; La diferencia es que si el nodo ya esta en el path, no lo vuleves a explorar.
  (if (nodo-in-path path)
      nil
    (mapcar #'(lambda(n)(cons n path))(rest (assoc node net)))))

(nodo-in-path 'e '( h b c d e))
(new-paths-improved '(a d) 'a '((a d) (b d f) (c e) (d f) (e b f) (f)))

(defun bfs-improved (end queue net)
	(if (null queue) '()
		(let* ((path (first queue))
			(node (first path)))
		(if (eql node end)
			(reverse path)
		  (bfs-improved end
		  	(append (rest queue)
		  		(new-paths-improved path node net))
		  	net)))))

(defun shortest-path-improved (start end net) (bfs-improved end (list (list start)) net))