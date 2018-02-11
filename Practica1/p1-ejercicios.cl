
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

(defun is-ok (x y) ;; Funcion que recorre la lista comprobando si los elementos son todos positivos y si ambas listas son de igual tama�o
  (cond ((and (null x) (null y)) T) ;; Comprobamos que ambas listas son distintas de nil -> T
        ( (and (null x) ;; Si x es nil e y no lo es -> nil
               (not (null y)))
         NIL)
        ( (and (null y) ;; Sy y es nil y x no lo es -> nil
               (not (null x)))
         NIL)
       ((every #'zerop x) nil) ;; Comprobamos si x e y son iguales al vector 0 -> nil
       ((every #'zerop y) nil)
       ((< (first x) 0) NIL) ;;Si el primer elemento de cada lista es negativo -> nil
       ((< (first y) 0) NIL) 
       (T (is-ok (rest x) (rest y))))) ;;Hacemos la llamado recursiva para recorrer ambas listas.

(is-ok '(1 2 3) '(3 4 5)) ;; t
(is-ok '(1 2 3) '(3 4 -5)) ;; nil
(is-ok '(1 2 3) '(0 0 0))  

(defun our-pesc-rec (x y) ;;Calcula el producto escalar de dos vectores representados como listas
  (if (or (equal nil x) (equal nil y)) ;; Si x o y es null devolvemos 0
      0
    (+ (* (first x) (first y)) (our-pesc-rec (rest x) (rest y)))))  ;; Calculamos sum ( x(i) * y(i)) con 1 < i < long x

(our-pesc-rec '(1 2 3) '( 2 5 6))
  
(defun sc-rec (lista1 lista2)
  (if (equal NIL (is-ok lista1 lista2)) ;;Comprobamos que a lista cumple las condiciones del enunciado
      NIL
    (/ (our-pesc-rec lista1 lista2) (*(sqrt (our-pesc-rec lista1 lista1)) (sqrt (our-pesc-rec lista2 lista2))))))
    ;; El producto escalar de un vector consigo mismo es su norma al cuadrado
(sc-rec '(1 0) '(0 1))
(sc-rec '() '(0 1))


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
  (reduce '+ (mapcar #'* lista1 lista2))) ;; Sum (x(i) * y(i)) com 1< i <len x

(defun sc-mapcar (lista1 lista2) ;; Utilizamos las funciones recursivas del primer apartado.
  (if (equal NIL (is-ok lista1 lista2))
      NIL
     (/ (our-pesc-map lista1 lista2) (*(sqrt (our-pesc-map lista1 lista1)) (sqrt (our-pesc-map lista2 lista2))))))

(sc-mapcar '(1 2 3) '(2 3 4))


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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;(defun our-similarity-cos (x vs)
;;;;;;(mapcar #'(lambda (y) (sc-mapcar x y)) vs))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun our-conf (x vs conf)
  (remove-if #'(lambda (y) (< (sc-mapcar x y) conf)) vs)) ;; Eliminamos de de vs las listas cuyo cos con x sea menor que una constante

(our-conf '(1 2 3) '((1 2 3) (2 3 4) (1 0 0)) 0.5)

(defun sc-conf (x vs conf)
  (sort (our-conf x vs conf) #'(lambda (z y) (> (sc-mapcar x z) (sc-mapcar x y))))) ;; Ordenamos de mayor a menor e vector en funci�n de su cos con la lista x.

(sc-conf '(1 2 3) '((1 2 3) (3 4 5) (1 0 0) (1 1 1)) 0.9)

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


(defun our-similarity-cos (cats lista func)
  (mapcar #'(lambda (y) (append(list(first y) (funcall func (rest lista) (rest y))))) cats))
  
(our-similarity-cos '((1 2 3) ( 2 3 4) (6 6 8)) '(3 3 3) #'sc-rec)

(defun our-max-similarity (cats lista func)
  (first (sort(our-similarity-cos cats lista func) #'(lambda (z y) (> (second z) (second y))))))

(our-max-similarity '((1 2 3) ( 2 3 5) (6 6 8)) '( 3 6 8) #'sc-rec)

(defun sc-classifier (cats texts func)
  (mapcar #'(lambda (z) (our-max-similarity cats z func)) texts))


(sc-classifier '((1 2 3) (2 3 5) (3 6 8)) '((1 3 5) (2 6 8)) #'sc-rec)
(sc-classifier '((1 2 3) (2 3 5) (3 6 8)) '((1 3 5) (2 3 6) (3 2 3)) #'sc-rec)
(sc-classifier '((1 2 3) (2 3 5) (3 6 8)) '((1 3 5) (2 6 8)) #'sc-mapcar)
(sc-classifier '((1 2 3) (2 3 5) (3 6 8)) '((1 3 5) (2 3 6) (3 2 3)) #'sc-mapcar)
(sc-classifier '((1 43 23 12) (2 33 54 24)) '((1 3 22 134) (2 43 26 58)) #'sc-rec) 

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

(defun our-distance (a b)
  (abs (- b a)))

(defun our-medium-point (a b)
  (/ (+ a b) 2))

 (defun bisect (f a b tol)
  (let ((c (our-medium-point a b)))
  (cond ((equal a 0) a)
        ((equal b 0) b)
        ((> (* (funcall f a) (funcall f b)) 0) nil)
        ((< (our-distance a b) tol) c)
        ((= c 0) c)
        ((< (* (funcall f a) (funcall f c)) 0) (bisect f a c tol))
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
  (if (or (equal lst nil) (equal (rest lst) nil)) ;; Caso base: Si la lista no tiene elementos o solo 1.
      nil
    (mapcan #'(lambda (x) (unless (null x) (list x))) ;;Lo utilizamos para eliminar los nil de la lista
    (append (list (bisect f (first lst) (second lst) tol))  ;; lista de soluciones
            (allroot f (rest lst) tol)))))

(allroot #'(lambda(x) (sin (* 6.28 x))) '(0.25 0.75 1.25 1.75 2.25) 0.0001)
(allroot #'(lambda(x) (sin (* 6.28 x))) '(0.25 0.9 0.75 1.25 1.75 2.25) 0.0001) 
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

(defun div-int (a b N)
  (let ((c (our-medium-point a b)))
    (if (equal N 0)
        (list a)
      (append (div-int a c (- N 1)) (div-int c b (- N 1))))))

(div-int 0 8 3)

(defun allind (f a b N tol) 
  (allroot f (append (div-int a b N) (list b)) tol))



(allind #'(lambda(x) (sin (* 6.28 x))) 0 3 2 0.01) ;
(allind #'(lambda(x) (sin (* 6.28 x))) 0.1 2.25 3 0.0001)
(allind #'(lambda(x) (sin (* 6.28 x))) 0.25 2.25 3 0.01) ;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 3.1
;;; combine-elt-lst (elem lst)
;;; Combina un elemento dado con todos los elementos de una lista
;;;
;;; INPUT: elt: elemento que se combinará con los de la lista
;;; lst: lista con la que se combinará el elemento
;;;
;;; OUTPUT: devuelve la lista con las combinaciones del elemento y la lista dadas.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun combine-elem-lst (elem lst)
  (let ((k (list(list elem (first lst)))))
  (if (equal lst nil)
      nil
    (if (equal (rest lst) nil)
        k
      (append 
       k 
       (combine-elem-lst elem (rest lst)))))))

(combine-elem-lst 'a nil)
(combine-elem-lst 'a '(1 2 3))

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

(defun combine-lst-lst (lst1 lst2)
  (if (or (equal lst1 nil) (equal lst2 nil))
      nil
    (append (combine-elem-lst (first lst1) lst2) (combine-lst-lst (rest lst1) lst2))))

(combine-lst-lst nil nil)
(combine-lst-lst '(a b c) nil)
(combine-lst-lst NIL '(a b c))
(combine-lst-lst '(a b c) '(1 2))
(combine-lst-lst '(a b c d e f) '(1 2))


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

(defun combine-list-of-lsts (lstolsts)
  (if (equal lstolsts nil)
      nil
    (if (member nil lstolsts)
        nil
     (combine-lst-lst (first lstolsts) (combine-list-of-lsts (rest lstolsts))))))
    
(combine-list-of-lsts '(() (+ -) (1 2 3 4)))
(combine-list-of-lsts '((a b c) () (1 2 3 4)))
(combine-list-of-lsts '((a b c) (1 2 3 4) ()))
(combine-list-of-lsts '((1 2 3 4)))
(combine-list-of-lsts '((a b c) (+ -) (1 2 3 4)))

(defun f (x y) (+ y x))

(defun mycall (f)
  (funcall f 2 3))

(mycall #'f)