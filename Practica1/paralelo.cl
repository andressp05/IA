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
(eliminate-biconditional '(<=>  (<=> p  q) (^ s (¬ q))))
;;   (^ (=> (^ (=> P Q) (=> Q P)) (^ S (¬ Q)))
;;      (=> (^ S (¬ Q)) (^ (=> P Q) (=> Q P))))

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
(eliminate-conditional '(=> p q))                      ;;; (V (¬ P) Q)
(eliminate-conditional '(=> p (v q s p)))              ;;; (V (¬ P) (V Q S P))
(eliminate-conditional '(=> (=> (¬ p) q) (^ s (¬ q)))) ;;; (V (¬ (V (¬ (¬ P)) Q)) (^ S (¬ Q)))

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
(reduce-scope-of-negation '(¬ (v p (¬ q) r))) 
;;; (^ (¬ P) Q (¬ R))
(reduce-scope-of-negation '(¬ (^ p (¬ q) (v  r s (¬ a))))) 
;;;  (V (¬ P) Q (^ (¬ R) (¬ S) A))

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
   ((cnf-p wff) wff) ;;si la FBF está ya en FNC la devuelve
   ((literal-p wff) ;; si la FBF es un literal
    (list +and+ (list +or+ wff))) ;; devuelve (v (^ wff))
   ((let ((connector (first wff))) ;; llamamos conector al primer elemento de la wff 
      (cond
       ((equal +and+ connector)  ;; si el conector es un v 
        (cons +and+ (simplify +and+ (mapcar #'cnf (rest wff))))) ;;devuelve una lista simplificada con ands 
       ((equal +or+ connector) ;; si el conector es un ^
        (cnf (exchange-NF (cons +or+ (simplify +or+ (rest wff))))))))))) ;;devuelve una lista simplificada con ors


(cnf 'a)

(cnf '(v (¬ a) b c))
(print (cnf '(^ (v (¬ a) b c) (¬ e) (^ e f (¬ g) h) (v m n) (^ r s q) (v u q) (^ x y))))
(print (cnf '(v (^ (¬ a) b c) (¬ e) (^ e f (¬ g) h) (v m n) (^ r s q) (v u q) (^ x y))))
(print (cnf '(^ (v p  (¬ q)) a (v k  r  (^ m  n)))))
(print (cnf '(v p  q  (^ r  m)  (^ n  a)  s )))
(exchange-NF '(v p  q  (^ r  m)  (^ n  a)  s ))
(cnf '(^ (v a b (^ y r s) (v k l)) c (¬ d) (^ e f (v h i) (^ o p))))
(cnf '(^ (v a b (^ y r s)) c (¬ d) (^ e f (v h i) (^ o p))))
(cnf '(^ (^ y r s (^ p q (v c d))) (v a b)))
(print (cnf '(^ (v (¬ a) b c) (¬ e) r s 
                (v e f (¬ g) h) k (v m n) d)))
;;
(cnf '(^ (v p (¬ q)) (v k r (^ m  n))))
(print  (cnf '(v (v p q) e f (^ r  m) n (^ a (¬ b) c) (^ d s))))
(print (cnf '(^ (^ (¬ y) (v r (^ s (¬ x)) (^ (¬ p) m (v c d))) (v (¬ a) (¬ b))) g)))
;;
;; EJEMPLOS:
;;
(cnf NIL)              ; NIL
(cnf 'a)               ; (^ (V A))
(cnf '(¬ a))           ; (^ (V (¬ A)))
(cnf '(V (¬ P) (¬ P))) ; (^ (V (¬ P) (¬ P)))
(cnf '(V A))           ; (^ (V A))
(cnf '(^ (v p (¬ q)) (v k r (^ m  n))))
;;;   (^ (V P (¬ Q)) (V K R M) (V K R N))
(print  (cnf '(v (v p q) e f (^ r  m) n (^ a (¬ b) c) (^ d s))))
;;; (^ (V P Q E F R N A D)      (V P Q E F R N A S)
;;;    (V P Q E F R N (¬ B) D)  (V P Q E F R N (¬ B) S)
;;;    (V P Q E F R N C D)      (V P Q E F R N C S) 
;;;    (V P Q E F M N A D)      (V P Q E F M N A S) 
;;;    (V P Q E F M N (¬ B) D)  (V P Q E F M N (¬ B) S) 
;;;    (V P Q E F M N C D)      (V P Q E F M N C S))
;;;
(print 
 (cnf '(^ (^ (¬ y) (v r (^ s (¬ x)) 
                      (^ (¬ p) m (v c d)))(v (¬ a) (¬ b))) g)))
;;;(^ (V (¬ Y)) (V R S (¬ P)) (V R S M) 
;;;   (V R S C D) (V R (¬ X) (¬ P)) 
;;;   (V R (¬ X) M) (V R (¬ X) C D)
;;;   (V (¬ A) (¬ B)) (V G))  

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
(eliminate-connectors (cnf '(^ (v p  (¬ q))  (v k  r  (^ m  n)))))
(eliminate-connectors
 (cnf '(^ (v (¬ a) b c) (¬ e) (^ e f (¬ g) h) (v m n) (^ r s q) (v u q) (^ x y))))

(eliminate-connectors (cnf '(v p  q  (^ r  m)  (^ n  q)  s )))
(eliminate-connectors (print (cnf '(^ (v p  (¬ q)) (¬ a) (v k  r  (^ m  n))))))

(eliminate-connectors '(^))
(eliminate-connectors '(^ (v p (¬ q)) (v) (v k r)))
(eliminate-connectors '(^ (v a b)))

;;   EJEMPLOS:
;;

(eliminate-connectors '(^ (v p (¬ q)) (v k r)))
;; ((P (¬ Q)) (K R))
(eliminate-connectors '(^ (v p (¬ q)) (v q (¬ a)) (v s e f) (v b)))
;; ((P (¬ Q)) (Q (¬ A)) (S E F) (B))

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
(wff-infix-to-cnf '(¬ a))
(wff-infix-to-cnf  '( (¬ p) v q v (¬ r) v (¬ s)))
(wff-infix-to-cnf  '((p v (a => (b ^ (¬ c) ^ d))) ^ ((p <=> (¬ q)) ^ p) ^ e))
;; ((P (¬ A) B) (P (¬ A) (¬ C)) (P (¬ A) D) ((¬ P) (¬ Q)) (Q P) (P) (E))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.1
;; Construye el conjunto de clausulas lambda-neutras para una FNC 
;;
;; RECIBE   : cnf    - FBF en FBF simplificada
;;            lambda - literal positivo
;; EVALUA A : cnf_lambda^(0) subconjunto de clausulas de cnf  
;;            que no contienen el literal lambda ni ¬lambda   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun extract-neutral-clauses (lambda cnf) 
  (set-difference cnf (list lambda) :test 'equal)
  )

;;
;;  EJEMPLOS:
;;
(extract-neutral-clauses 'p
                           '((p (¬ q) r) (p q) (r (¬ s) q) (a b p) (a (¬ p) c) ((¬ r) s)))
;; ((R (¬ S) Q) ((¬ R) S))


(extract-neutral-clauses 'r NIL)
;; NIL

(extract-neutral-clauses 'r '(NIL))
;; (NIL)

(extract-neutral-clauses 'r
                           '((p (¬ q) r) (p q) (r (¬ s) q) (a b p) (a (¬ p) c) ((¬ r) s)))
;; ((P Q) (A B P) (A (¬ P) C))

(extract-neutral-clauses 'p
                           '((p (¬ q) r) (p q) (r (¬ s) p q) (a b p) (a (¬ p) c) ((¬ r) p s)))
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
(defun extract-positive-clauses (lambda cnf) 
  (set-difference cnf (list lambda) :test 'equal)
  )

;;
;;  EJEMPLOS:
;;
(extract-positive-clauses 'p
                             '((p (¬ q) r) (p q) (r (¬ s) q) (a b p) (a (¬ p) c) ((¬ r) s)))

;; ((P (¬ Q) R) (P Q) (A B P))


(extract-positive-clauses 'r NIL)
;; NIL
(extract-positive-clauses 'r '(NIL))
;; NIL
(extract-positive-clauses 'r
                             '((p (¬ q) r) (p q) (r (¬ s) q) (a b p) (a (¬ p) c) ((¬ r) s)))
;; ((P (¬ Q) R) (R (¬ S) Q))
(extract-positive-clauses 'p
                             '(((¬ p) (¬ q) r) ((¬ p) q) (r (¬ s) (¬ p) q) (a b (¬ p)) ((¬ r) (¬ p) s)))
;; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.3
;; Construye el conjunto de clausulas lambda-negativas para una FNC 
;;
;; RECIBE   : cnf    - FBF en FNC simplificada
;;            lambda - literal positivo 
;; EVALUA A : cnf_lambda^(-) subconjunto de clausulas de cnf  
;;            que contienen el literal ¬lambda  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun extract-negative-clauses (lambda cnf) 
  ;;
  ;; 4.4.3 Completa el codigo
  ;;
  )

;;
;;  EJEMPLOS:
;;
(extract-negative-clauses 'p
                             '((p (¬ q) r) (p q) (r (¬ s) q) (a b p) (a (¬ p) c) ((¬ r) s)))
;; ((A (¬ P) C))

(extract-negative-clauses 'r NIL)
;; NIL
(extract-negative-clauses 'r '(NIL))
;; NIL
(extract-negative-clauses 'r
                             '((p (¬ q) r) (p q) (r (¬ s) q) (a b p) (a (¬ p) c) ((¬ r) s)))
;; (((¬ R) S))
(extract-negative-clauses 'p
                             '(( p (¬ q) r) ( p q) (r (¬ s) p q) (a b p) ((¬ r) p s)))
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
(defun resolve-on (lambda K1 K2) 
  ;;
  ;; 4.4.4 Completa el codigo
  ;;
  )

;;
;;  EJEMPLOS:
;;
(resolve-on 'p '(a b (¬ c) p) '((¬ p) b a q r s))
;; (((¬ C) B A Q R S))

(resolve-on 'p '(a b (¬ c) (¬ p)) '( p b a q r s))
;; (((¬ C) B A Q R S))

(resolve-on 'p '(p) '((¬ p)))
;; (NIL)


(resolve-on 'p NIL '(p b a q r s))
;; NIL

(resolve-on 'p NIL NIL)
;; NIL

(resolve-on 'p '(a b (¬ c) (¬ p)) '(p b a q r s))
;; (((¬ C) B A Q R S))

(resolve-on 'p '(a b (¬ c)) '(p b a q r s))
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