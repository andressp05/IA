 
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
(defun our-length (lista) ;;Calcula la longitud de una lista
  (if (equal nil lista)
      0
      (+ 1 (our-length (rest lista)))))

(defun our-neg (lista) ;;Calcula si todos los numero de una lista son positivos
  (some #'minusp lista)) ;;Devuelve True si hay alguno negativo, NIL en caso contrario

(defun our-pesc (x y) ;;Calcula el producto escalar de dos vectores representados como listas
  (if (or (equal nil x) (equal nil y))
      0
    (+ (* (first x) (first y)) (our-pesc (rest x) (rest y))))) 
  
(defun our-norm-square (x) ;;Calcula la norma al cuadrado de un vector
  (if (equal nil x)
      0
    (+ (* (first x) (first x)) (our-norm-square (rest x)))))
  
(defun sec-rec (lista1 lista2)
  (if (or (and(our-neg lista1) T) (and(our-neg lista1)) (/= (our-length lista1) (our-length lista2)))
      0
    (/ (our-pesc lista1 lista2) (*(sqrt (our-norm-square lista1)) (sqrt (our-norm-square lista2))))))


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

(defun p-escalar (lista1 lista2)
  (reduce '+ (mapcar #'* lista1 lista2)))

(defun norma (lista)
  (if (equal lista nill)
      0
    (reduce '+ (mapcar #'(lambda (x) (* x x)) lista))))

(defun sc-mapcar (x y) ) ;; LO hago ma�anaaa!!!

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

(defun sc-conf (x vs conf) ...)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 1.3
;;; sc-classifier (cats texts func)
;;; Clasifica a los textos en categorías.
;;;
;;; INPUT: cats: vector de vectores, representado como una lista de listas
;;; vs: vector de vectores, representado como una lista de listas
;;; func: referencia a función para evaluar la similitud coseno
;;;
;;; OUTPUT: Pares identificador de categoría con resultado de similitud coseno
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sc-classifier (cats texts func) ...)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 2.1
;;; bisect (f a b tol)
;;; Encuentra una raíz de f entre los puntos a y b usando bisección
;;;
;;; Si f(a)f(b)>0 no hay garantía de que vaya a haber una raíz en el
;;; intervals, y la función devolverá NIL.
;;;
;;; INPUT: f: función de un solo parámetro real con valores reales cuya
;;; raíz queremos encontrar
;;; a: extremo inferior del intervalo en el que queremos buscar la raíz
;;; b: b>a extremo superior del intervalo en el que queremos buscar la raíz
;;; tol: tolerancia para el criterio de parada: si b-a < tol de la función
;;;
;;; OUTPUT: devuelve (a+b)/2 como solución
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bisect (f a b tol) ...)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 2.2
;;; allroot (f lst tol)
;;; Encuentra todas las raíces localizadas entre dos valores consecutivos
;;; de una lista de valores
;;;
;;; INPUT: f: función de un solo parámetro real con valores reales cuya
;;; raíz queremos encontrar
;;; lst: lista ordenada de valores reales (lst[i] < lst[i+1])
;;; tol: tolerancia para el critreio de parada: si b-a < tol de la función
;;;
;;; OUTPUT: una lista o valores reales que contienen las raíces de la función
;;; en los subintervalos dados.
;;;
;;; Cuando sgn(f(lst[i])) != sgn(f(lst[i+1])) esta función busca
;;; una raíz en el correspondiente intervalo 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun allroot (f lst tol) ...)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 2.3
;;; allind (f a b N tol)
;;;	Divide en un número 2^N de invervalos y encuentra todas las raíces
;;; de la función f en los intervalos obtenidos
;;;
;;; INPUT: f: función de un solo parámetro real con valores reales cuya
;;; raíz queremos encontrar
;;; a: extremo inferior del intervalo en el que buscamos la raíz
;;; b: b>a extremo superior del intervalo en el que queremos buscar la raíz
;;; N: exponente del número de intervalos en el que [a, b] va a ser dividido
;;; [a, b] es dividido en 2^N intervalos
;;; tol: tolerancia para el critreio de parada: si b-a < tol de la función
;;;
;;; OUTPUT: devuelve (a+b)/2 como solución
;;;
;;; El intervalo (a, b) es dividido en intervalos (x[i], x[i+1]) con
;;; x[i] = a + i*dlt; una raíz es buscada en cada intervalo, y todas las
;;; raíces encontradas se juntan en una lista que se devuelve
;;;
;;; Pista:
;;; Uno puede encontrar una manera de usar allroot para implementar esta función.
;;; Esto es posible por supuesto, pero hay una forma simple de hacerlo recursivo
;;; sin usar allroot.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun allind (f a b N tol) ...)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 3.1
;;; combine-elt-lst (elt lst)
;;; Combina un elemento dado con todos los elementos de una lista
;;;
;;; INPUT: elt: elemento que se combinará con los de la lista
;;; lst: lista con la que se combinará el elemento
;;;
;;; OUTPUT: devuelve la lista con las combinaciones del elemento y la lista dadas.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun combine-elt-lst (elt lst) ...)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 3.2
;;; combine-lst-lst (lst1 lst2)
;;; Calcula el producto cartesiano de dos listas
;;;
;;; INPUT: lst1: primera lista sobre la que se realizará el producto cartesiano
;;; lst2: segunda lista sobre la que se realizará el producto cartesiano
;;;
;;; OUTPUT: devuelve la lista resultado del producto cartesiando de las anteriores
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun combine-lst-lst (lst1 lst2) ...)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 3.3
;;; combine-list-of-lsts (lstolsts)
;;; Calcula todas las posibles disposiciones pertenecientes a N listas 
;;; de forma que en cada disposición aparezca solo un elemento de cada lista
;;;
;;; INPUT: lstolsts: todas las listas que se combinarán
;;;
;;; OUTPUT: devuelve una lista resultado de la combinación de todas las dadas
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun combine-list-of-lsts (lstolsts) ...)
