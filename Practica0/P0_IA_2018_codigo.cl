;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; EJEMPLO 1
;;;
;;; Expresiones y evaluaciones (pag. 1-2)
;;;     
;;;	En este código encontrarás una función 
;;;	adicional list que forma una lista con los 
;;;	elementos que se le pasan. Explica los 
;;;	resultados obtenidos, con especial hincapié 
;;;	en las sentencias con un doble eval
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf a (+ 2 3)) 				; a = 5
(eval a)

(setf b '(+ 2 3)) 				; b = (+23)
(eval b)						; (eval b) = 5

(setf c (quote (+ 2 3))) 		; c = (+23)
(eval c)						; (eval c) = 5

(setf d (list 'quote '(+ 2 3))) ; d = '(+23)
(eval d) 						; (eval d) = (+23)
(eval (eval d))					; (eval (eval d)) = 5

(setf e (list 'quote (+ 2 3)))	; e = '5
(eval e)						; (eval e) = 5
(eval (eval e))					; (eval e) = 5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; EJEMPLO 2
;;;
;;; Predicados (pag. 2)
;;;
;;;	Comenta brevemente los resultados obtenidos 
;;;	de evaluar las siguientes expresiones
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(atom 12) ; True
(atom 'abc) ; True
(atom a)	; True
(atom '(a b)) ;False
(atom '()) ;True

(null '()) ; True
(null nil) ; True

(symbolp 12) ; False
(symbolp 'abc) ; True
(symbolp a) ; False
(symbolp nil) ; True

(numberp 123) ;True
(numberp 'a) ;False
(setf a 3)
(numberp a) ; True

(listp '(a b)) ; True
(listp '()) ; True	
(listp nil) ; True
(listp '(+ 2 3)) ; True

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; EJEMPLO 3
;;;
;;; Estructura y funciones de listas (pag. 2-3)
;;;
;;;	Ejecuta y comprende el siguiente código y 
;;;	plantea el pseudocódigo de una implementación 
;;;	propia de 'length' que utilice recursión sobre
;;;	la lista pasada. Llama a esta funcion 
;;;	'my-length'
;;;	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf milista nil) ; NIL
(setf milista '(- 6 2)) ; (- 6 2)
(setf milista (- 6 2)) ; 4
(setf milista '(8 (9 10) 11)) ; (8 (9 10) 11)
(car milista) ; 8
(first milista) ;8
(cdr milista) ; ((9 10 ) 11)
(rest milista) ; ((9 10 ) 11)
(car (cdr milista)) ;(9 10)
(length milista) ; 3 [8 (9 10) 11]
(setf milista (cons 4 '(3 2))) ; (4 3 2)
(setf milista (list '+ 4 3 2)) ; (+ 4 3 2)
(eval milista)				   ; 9
(setf milista (append '(4 3) '(2 1))) ; (4 3 2 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; EJEMPLO 4
;;;
;;; Operadores lógicos y condicionales (pag. 3-4)
;;;
;;;	Evalua el siguiente código y asegurate de 
;;;	entender por qué cada caso devuelve lo que 
;;;	devuelve. Prueba a modificar el valor de nota
;;;	para que el cond devuelva los otros casos. 
;;;	Prueba también a modificar el cond para que 
;;;	devuelva nil
;;;	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if t 1 2) ; 1
(if nil 1 2) ; 2
(when t 1 2 3) ; 3
(when nil 1 2 3) ; NIL
(unless t 1 2 3) ; NIL
(unless nil 1 2 3) ; 3
(setf nota 7) ; nota = 7
(cond ((<= nota 5) 'suspenso)
      ((<= nota 7) 'aprobado)
      ((<= nota 9) 'notable) ; Aprobado
       (t 'sobresaliente))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; EJEMPLO 5
;;;
;;; Definición de funciones
;;;
;;;	Evalua el siguiente código y asegurate de 
;;;	entender cómo funcionan las funciones ahí
;;;	definidas. A continuación, implementa en LISP
;;;	la función 'my-length' cuyo pseudocódigo 
;;;	escribiste anteriormente, y comprueba su 
;;;	correcto funcionamiento mediante diferentes 
;;;	pruebas.
;;;	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun elimina-primero (lista)
  (rest lista))
(setf lista '(1 2 3 4 5 6 7)) 
(elimina-primero lista) ; (2 3 4 5 6 7)
lista ; (1 2 3 4 5 6 7)

(defun elimina-segundo (lista)
  (cons (first lista)
        (rest (rest lista)))) ; Cogemos la lista (1) y (3 4 5 6 7) y las unimos con cons.
(setf lista '(1 2 3 4 5 6 7)) 
(elimina-segundo lista) ; (1 3 4 5 6 7)
lista ; (1 2 3 4 5 6 7)

(defun elimina-enesimo (lista n)
  (if (<= n 1)
      (rest lista)
      (cons (first lista) 
            (elimina-enesimo (rest lista) (- n 1)))))
(setf lista '(1 2 3 4 5 6 7)) 
(elimina-enesimo lista 4)
lista

(defun our-length (lista)
	(if (equal nil lista) 
      (0) 
	  (+ 1 (our-length (rest lista)))))
(setf lista '(1 2 3 4 5 6 7))
(our-length lista)
lista

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; EJEMPLO 6
;;;
;;; Aplicación repetida de funciones (pag. 4-5)
;;;
;;;	Una vez hayas comprendido el siguiente código,
;;;	implementa una función 'sum-range' que reciba 
;;;	un número n y devuelva el resultado de sumar
;;;	todos los números desde 1 hasta n. Por 
;;;	ejemplo, (sum-range 10) debe dar 55 y 
;;;	(sum-range 100) es 5050. Si el número pasado 
;;;	no es positivo haz que 'sum-range' devuelva 
;;;	nil. Puedes definir funciones auxiliares si 
;;;	te facilita la tarea
;;;	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mapcar #'(lambda (x) (* x x)) '(1 2 3)) ; Aplica a la lista la operacion definida
(setf sqr (lambda (x) (* x x))) ; sqr(x) = x * x
(mapcar sqr '(1 2 3)) 
(maplist #'append '(1 2 3 4) '(1 2) '(1 2 3)) ; Tantas listas como el menor tamaño de lista de las de argumentos de entrada
(funcall sqr 3)
(apply sqr '(3))
(apply #'+ '(1 2 3 4)) ; 1 + 2 + 3 + 4 

(defun sum-range (n)
	(if (eq n 0)  (0)  (+ n (sum-range(- n 1))))) ;;FUNCIONA!! cambiar orden 1 n a n 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; EJEMPLO 7
;;;
;;; Comparaciones y búsquedas (pag. 5-6)
;;;
;;;	Estudia el siguiente código y razona las 
;;;	semejanzas y las diferencias entre los 
;;;	resultados obtenidos con eq, eqly equal. 
;;;	
;;;	Una vez entendidas, implementa una versión 
;;;	propia de 'member' llamada 'my-member' que 
;;;	reciba 3 argumentos: el elemento, la lista
;;;	el comparador a usar. Prueba con distintos 
;;;	ejemplos tanto con 'eql' como con 'equal', 
;;;	y asegúrate de que devuelve lo mismo que el 
;;;	'member' nativo con el test adecuado.
;;;	
;;;	Cuando hayas comprobado su corrección, 
;;;	codifica una función  'my-count' que haga uso 
;;;	de 'my-member'. De nuevo, comprueba que 
;;;	obtienes los mismos resultados que el 
;;;	'count' nativo.
;;;	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf a 3)
(setf b 3)
(eq a a) ;T
(eq a 3) ;T
(eq a b) ;T
(eq a 3.0) ;Nil
(eql a 3) ;T
(eql a b) ;T
(eql a 3.0) ;Nil
(equal a 3) ;T
(equal a b) ;T
(equal a 3.0) ;NIL
(= a 3) ;T
(= a b) ;T
(= a 3.0) ;T!!!

(setf lst '(1 2 3))
(setf lst2 '(1 2 3))
(eq lst lst) ;T
(eq lst '(1 2 3)) ;Nil
(eq lst lst2) ;Nil
(eql lst '(1 2 3)) ; Nil
(eql lst lst2) ; Nil
(equal lst '(1 2 3)) ;T
(equal lst lst2) ;T

(member 2 lst) ;(2 3) Devuelve la sublista cuyo primer el elemento es el buscado
(member-if #'oddp lst) ;(1 2 3) En vez de comprobar igualdad, comprueba la condición que se le pasa
(position 1 lst) ; 0 Determina la primera posicionen la que se encuentra el elemento a buscar
(position-if #'zerop lst) ;Nil (Posicion del primer 0)
(remove 3 lst) ;(1 2) Elimina el elemeto 3. No modifica lst!!!!!!!
(remove-if #'evenp lst) ; (1 3) 
(every #'numberp lst) ; T Comprueba si todos los elementos de la lista cumplen una condicion determinada.
(some #'minusp lst) ; Nil Comprueba si algun elemento cumple la condicion

(defun my-member (elemento lista comparador) 
	(if ((comparador elemento (first lista)) and (unless lista nill) (lista) (my-member (elemento) (rest lista) (comparador))))

(defun my-count (elemento lista comparador) (if ((eq (my-member (elemento) (lista) (comparador)) (lista)) and (not (eq lista nil))) (+ 1 (my-member (elemento) (rest lista) (comparador))) (my-member (elemento) (rest lista) (comparador))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; EJEMPLO 8
;;;
;;; Comparación de implementaciones (pag. 6)
;;;
;;;	Estudia el siguiente código donde se dan tres
;;;	versiones de una función que obtiene los 
;;;	elementos pares de una lista. Comenta las 
;;;	ventajas e inconvenientes que le ves a cada 
;;;	una de esas implementaciones.
;;;	
;;;	Comprueba la eficiencia de estas 
;;;	implementaciones. Para ello, construye una 
;;;	lista grande con ayuda de la función 'make-list'
;;;	(por ejemplo de tamaño 1000) y mide tiempos de
;;;	ejecución haciendo uso de la función 'time'.
;;;	Analiza los resultados obtenidos.
;;;	
;;;	A la vista de estos resultados, ¿crees que 
;;;	podrías implementar más eficientemente 
;;;	'my-length' y 'my-member'? En caso de que sí, 
;;;	prueba a reimplementarlas y compara 
;;;	ejecuciones con time a ver si estás en lo 
;;;	cierto. 
;;;	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; obten-pares: Implementación con remove-if
;;;

(setf lista '(1 2 4 5 0 6 8 7 4))

(defun obten-pares (lista)  
  (remove-if #'oddp lista)) 

(obten-pares lista)

;; Facil implementar pero no modifica la lista inicial ni se puede acceder a ella
;; Usa un comparador potente interno como oddp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; obten-pares: Implementación recursiva
;;;

(defun obten-pares-recursiva (lista)  
  (unless (null lista) 
    (let ((primero (first lista)))
      (if (evenp primero)
        (cons primero (obten-pares-recursiva (rest lista)))
        (obten-pares-recursiva (rest lista))))))

(obten-pares-recursiva lista)

;;Difícil de implementar
;; Consiste en ir comprobando elemento a elemento de la lista si son o no pares
;; si lo son se van almacenando

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; obten-pares: Implementación con mapcar
;;;

(defun obten-pares-mapcar (lista)  
  (remove NIL 
          (mapcar #'(lambda (x) 
                      (when (evenp x) x))
            lista)))

(obten-pares-mapcar lista)

;; Nivel intermedio
;; Crea una función que añade elementos a la lista si son pares

;; (time (obten-pares (make-list 1000 :initial-element 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  EJEMPLO 9
;;;
;;;  Funciones destructivas (pag. 6-7)
;;;
;;;	Estudia el siguiente código y verifica cómo 
;;;	las funciones destructivas modifican los
;;;	argumetnos que se les pasan, mientras que las
;;;	no destructivas no lo hacen.
;;;	
;;;	Codifica una función 'sorted-occurrences' que
;;;	cuente el número de veces que aparece cada 
;;;	elemento distinto en una lista. Debe devolver 
;;;	pares de la forma (elemento, número de 
;;;	ocurrencias), donde el orden es de mayor a 
;;;	menor según el número de ocurrencias. Por 
;;;	ejemplo, (sorted-occurrences '(3 1 4 2 3 1 3 
;;;	2 3 1)) debe devolver ((3 4) (1 3) (2 2) 
;;;	(4 1)). Asegúrate de que la función no es 
;;;	destructiva.
;;;	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; remove (no destructiva) / delete (destructiva)
;;;

(setf *lista-amigos* '(jaime maria julio))

(setf *lista-amigos-masculinos* (remove 'maria *lista-amigos*))
*lista-amigos* ;;(JAIME MARIA JULIO)

(setf *lista-amigos-masculinos* (delete 'maria *lista-amigos*))
*lista-amigos* ;; (JAIME JULIO)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; cons (no destructiva) / push (destructiva)
;;; first (no destructiva) / pop (destructiva)
;;;

(setf *dias-libres* '(domingo))

(cons 'sabado *dias-libres*)
*dias-libres* ; (DOMINGO)

(push 'sabado *dias-libres*)
*dias-libres* ; (SABADO DOMINGO)

(setf *dias-libres* (cons 'viernes *dias-libres*))
*dias-libres* ; (VIERNES SABADO DOMINGO)

(first *dias-libres*) ;(VIERNES)
*dias-libres* ; (VIERNES SABADO DOMINGO)

(pop *dias-libres*)
*dias-libres* ; (SABADO DOMINGO)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Ordenación de una lista por el cadr (second) de 
;;;  sus elementos (versión no destructiva)
;;;

(setf lst '((a -4) (b -3)  (c 1)  (d 9)))
(sort (copy-list lst)   ; copia solo el esqueleto de la lista
      #'(lambda(x y) (< (abs x) (abs y))) ; compara valor abs 
      :key #'second)                        ; del cadr
      ;;   ((C 1) (B -3) (A -4) (D 9))
lst ;; ((A -4) (B -3) (C 1) (D 9))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Ordenación de una lista por el cadr de sus 
;;;  elementos (versión destructiva)
;;;

(setf lst '((a -4) (b -3)  (c 1)  (d 9)))
(sort lst
      #'(lambda(x y) (< (abs x) (abs y))) ; compara valor abs 
      :key #'second)                        ; del cadr
      ;;   ((C 1) (B -3) (A -4) (D 9))
lst ;; ((B -3) (A -4) (D 9))

;; TE LA DEJO PARA TI GUAPO (11:10)
(defun sorted-occurrences (lista) (sort (copy-list lista) (if ((eq (find ((first lista) ocurrencias)) nil) and (eq lista nil)) ((cons '((first prr) (count ((first prr) lista))) ocurrencias) and ()) ()