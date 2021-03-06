
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 1.1.1
;;; sc-rec (x y)
;;; Calcula la similitud coseno de un vector de forma recursiva
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
;;;	sc-conf (x vs conf)
;;; Devuelve aquellos vectores similares a una categoria
;;;
;;; INPUT: x: vector, representado como una lista
;;; vs: vector de vectores, representado como una lista de listas
;;; conf: Nivel de confianza
;;;
;;; OUTPUT: Vectores cuya similitud es superior al nivel de confianza, ordenados
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun our-conf (x vs conf)
  (remove-if #'(lambda (y) (< (sc-mapcar x y) conf)) vs))     ;; Eliminamos de vs las listas cuyo cos con x sea menor que una constante dada

(our-conf '(1 2 3) '((1 2 3) (2 3 4) (1 0 0)) 0.5)
(sc-mapcar '(1 2 3) '(1 0 0))

(defun sc-conf (x vs conf)
  (sort (our-conf x vs conf)
        #'(lambda (z y) (> (sc-mapcar x z) (sc-mapcar x y))))) ;; Ordenamos de mayor a menor el vector en funcion de su cos con la lista x.

(sc-conf '(1 2 3) '((1 2 3) (3 4 5) (1 0 0) (1 1 1)) 0.9)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 1.3
;;; sc-classifier (cats texts func)
;;; Clasifica a los textos en categorias.
;;;
;;; INPUT: cats: vector de vectores, representado como una lista de listas
;;; vs: vector de vectores, representado como una lista de listas
;;; func: referencia a funcion para evaluar la similitud coseno
;;;
;;; OUTPUT: Pares identificador de categori�a con resultado de similitud coseno
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

;; Los resultados obtenidos se comentar�n en la memoria
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 2.1
;;; bisect (f a b tol)
;;; Encuentra una raiz de f entre los puntos a y b usando biseccion
;;;
;;; Si f(a)f(b)>0 no hay garanti�a de que vaya a haber una rai�z en el
;;; intervalo, y la funcion devolvera NIL.
;;;
;;; INPUT: f: funcion de un solo parametro real con valores reales cuya
;;; raiz queremos encontrar
;;; a: extremo inferior del intervalo en el que queremos buscar la raiz
;;; b: b>a extremo superior del intervalo en el que queremos buscar la rai�z
;;; tol: tolerancia para el criterio de parada: si b-a < tol de la funcion
;;;
;;; OUTPUT: devuelve (a+b)/2 como solucion
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
;;; rai�z queremos encontrar
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
;;; a: extremo inferior del intervalo en el que buscamos la rai�z
;;; b: b>a extremo superior del intervalo en el que queremos buscar la rai�z
;;; N: exponente del numero de intervalos en el que [a, b] va a ser dividido
;;; [a, b] es dividido en 2^N intervalos
;;; tol: tolerancia para el critreio de parada: si b-a < tol de la funcion
;;;
;;; OUTPUT: devuelve (a+b)/2 como solucion
;;;
;;; El intervalo (a, b) es dividido en intervalos (x[i], x[i+1]) con
;;; x[i] = a + i*dlt; una raiz es buscada en cada intervalo, y todas las
;;; rai�ces encontradas se juntan en una lista que se devuelve
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
