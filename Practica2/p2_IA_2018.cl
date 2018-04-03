;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Lab assignment 2: Search
;;    LAB GROUP: 2301
;;    Couple:  08
;;    Author 1: Andres Salas
;;    Author 2: Ricardo Riol
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Problem definition
;;
(defstruct problem
  states               ; List of states
  initial-state        ; Initial state
  f-h                  ; reference to a function that evaluates to the 
                       ; value of the heuristic of a state
  f-goal-test          ; reference to a function that determines whether 
                       ; a state fulfils the goal 
  f-search-state-equal ; reference to a predictate that determines whether
                       ; two nodes are equal, in terms of their search state      
  operators)           ; list of operators (references to functions) to 
                       ; generate successors
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Node in search tree
;;
(defstruct node 
  state           ; state label
  parent          ; parent node
  action          ; action that generated the current node from its parent
  (depth 0)       ; depth in the search tree
  (g 0)           ; cost of the path from the initial state to this node
  (h 0)           ; value of the heurstic
  (f 0))          ; g + h 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Actions 
;;
(defstruct action
  name              ; Name of the operator that generated the action
  origin            ; State on which the action is applied
  final             ; State that results from the application of the action
  cost )            ; Cost of the action
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Search strategies 
;;
(defstruct strategy
  name              ; name of the search strategy
  node-compare-p)   ; boolean comparison
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    END: Define structures
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    BEGIN: Define galaxy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *planets* '(Avalon Davion Katril Kentares Mallory Proserpina Sirtis))

(defparameter *white-holes*  
  '((Avalon Mallory 6.4) (Avalon Proserpina 8.6)
    (Mallory Katril 10) (Mallory Proserpina 15)
    (Katril Mallory 10) (Katril Davion 9)
    (Kentares Katril 10) (Kentares Avalon 3) (Kentares Proserpina 7)
    (Proserpina Avalon 8.6) (Proserpina Davion 5) (Proserpina Mallory 15) (Proserpina Sirtis 12)
    (Davion Proserpina 5) (Davion Sirtis 6)
    (Sirtis Davion 6) (Sirtis Proserpina 12)
    ))

(defparameter *worm-holes*  
  '((Avalon Kentares 4) (Avalon Mallory 9)
    (Davion Katril 5) (Davion Sirtis 8)  
    (Kentares Avalon 4) (Kentares Proserpina 12)
    (Mallory Avalon 9) (Mallory Katril 5) (Mallory Proserpina 11)
    (Katril Mallory 5) (Katril Davion 5) (Katril Sirtis 10)
    (Proserpina Kentares 12) (Proserpina Mallory 11) (Proserpina Sirtis 9)
    (Sirtis Katril 10) (Sirtis Davion 8) (Sirtis Proserpina 9)
    ))
 
(defparameter *sensors* 
  '((Avalon 15) (Davion 5) (Mallory 12) (Kentares 14) (Proserpina 7) (Katril 9) (Sirtis 0)))

(defparameter *planet-origin* 'Mallory)
(defparameter *planets-destination* '(Sirtis))
(defparameter *planets-forbidden*   '(Avalon))
(defparameter *planets-mandatory*   '(Katril Proserpina))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         1. MODELING PROBLEM                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; BEGIN: Exercise 1 -- Evaluation of the heuristic
;;
;; Returns the value of the heuristics for a given state
;;
;; F-H-GALAXY
;;
;;  Input:
;;    state: the current state (vis. the planet we are on)
;;    sensors: a sensor list, that is a list of pairs
;;                (state cost)
;;             where the first element is the name of a state and the second
;;             a number estimating the cost to reach the goal
;;
;;  Returns:
;;    The cost (a number) or NIL if the state is not in the sensor list
;;

(defun f-h-galaxy (state sensors)  
  (second (assoc state sensors)))

(f-h-galaxy 'Sirtis *sensors*) ;-> 0
(f-h-galaxy 'Avalon *sensors*) ;-> 15
(f-h-galaxy 'Earth  *sensors*) ;-> NIL


;;
;; END: Exercise 1 -- Evaluation of the heuristic
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 2 -- Navigation operators
;;
;; Returns a list of actions that can be done with the state given

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NAVIGATE-WHITE-HOLE
;;
;;  Input:
;;    state: the current state (vis. the planet we are on)
;;    white-holes: list of all white-holes
;;
;;  Returns:
;;    A specified list with Navigate-white-whole as name,
;;    the state as origin and a final node with his cost associated
;;

(defun navigate-white-hole (state white-holes)
  (let ((aux (first white-holes)))
    (cond
     ((null white-holes) nil)
     ((equal (first aux) state) (cons (make-action :name 'Navigate-white-whole
                                                   :origin state
                                                   :final (second aux)
                                                   :cost (third aux))
                                      (navigate-white-hole state (rest white-holes))))
     (t (navigate-white-hole state (rest white-holes))))))

        
(navigate-white-hole 'Kentares *white-holes*) ; Lo hace bien

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NAVIGATE-WORM-HOLE
;;
;;  Input:
;;    state: the current state (vis. the planet we are on)
;;    worm-holes: list of all worm-holes
;;    planets-forbidden: list of forbidden planets
;;
;;  Returns:
;;    A specified list with Navigate-worm-whole as name,
;;    the state as origin and a final node with his cost associated
;;

(defun navigate-worm-hole (state worm-holes planets-forbidden)
  (let ((aux (first worm-holes)))
    (cond
     ((null worm-holes) nil)
     ((and (equal (first aux) state) (not(find (second aux) planets-forbidden :test #'equal)))
      (cons (make-action :name 'Navigate-worm-whole
                         :origin state
                         :final (second aux)
                         :cost (third aux))
            (navigate-worm-hole state (rest worm-holes) planets-forbidden)))
     (t (navigate-worm-hole state (rest worm-holes) planets-forbidden)))))


(defun navigate (state wholes planets-forbidden)
  (append (navigate-white-hole state wholes) (navigate-worm-hole state wholes planets-forbidden)))


(navigate-worm-hole 'Mallory *worm-holes* *planets-forbidden*)  ;-> 
;;;(#S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL KATRIL :COST 5)
;;; #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL PROSERPINA :COST 11))

(navigate-worm-hole 'Mallory *worm-holes* NIL)  ;-> 
;;;(#S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL AVALON :COST 9)
;;; #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL KATRIL :COST 5)
;;; #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL PROSERPINA :COST 11))


(navigate-white-hole 'Kentares *white-holes*) ;->
;;;(#S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN KENTARES :FINAL AVALON :COST 3)
;;; #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN KENTARES :FINAL KATRIL :COST 10)
;;; #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN KENTARES :FINAL PROSERPINA :COST 7))


(navigate-worm-hole 'Uranus *worm-holes* *planets-forbidden*)  ;-> NIL



;;
;; END: Exercise 2 -- Navigation operators
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 3A -- Goal test
;;
;; Returns wheter the goal is reached or not
;;
;; F-GOAL-TEST-GALAXY
;;
;;  Input:
;;    node: origin node
;;    planets-destination: list of all destinations planets 
;;    planets-mandatory: list of planets that should be used
;;
;;  Returns:
;;    T if is reached or NIL if not
;;

(defun f-goal-test-galaxy (node planets-destination planets-mandatory) 
  (and
   (find (node-state node) planets-destination :test #'equal)
   (f-goal-test-galaxy-aux node planets-mandatory)))

(defun f-goal-test-galaxy-aux (node planets-mandatory)
  (if (null node)
      (null planets-mandatory)
    (f-goal-test-galaxy-aux (node-parent node)
                            (remove (find (node-state node) planets-mandatory :test #'equal)
                                    planets-mandatory :test #'equal))))


(defparameter node-01
  (make-node :state 'Avalon) )
(node-state node-01)
(defparameter node-02
   (make-node :state 'Kentares :parent node-01))
(defparameter node-03
   (make-node :state 'Katril :parent node-02))
(defparameter node-04
   (make-node :state 'Kentares :parent node-03))
(f-goal-test-galaxy node-01 '(kentares urano) '(Avalon Katril)); -> NIL
(f-goal-test-galaxy node-02 '(kentares urano) '(Avalon Katril)); -> NIL
(f-goal-test-galaxy node-03 '(kentares urano) '(Avalon Katril)); -> NIL
(f-goal-test-galaxy node-04 '(kentares urano) '(Avalon Katril)); -> T


;;
;; END: Exercise 3A -- Goal test
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 3B  -- Equal predicate for search states
;;
;; Returns wheter the search-state is repeated or not
;;
;; F-SEARCH-STATE-EQUAL-GALAXY
;;
;;  Input:
;;    node-1: first node of the comparison
;;    node-2: second node of the comparison
;;    planets-mandatory: list of planets that should be used 
;;
;;  Returns:
;;    T if is repeated or NIL if not
;;

;; Con la funcion f-goal-test-galaxy-aux devolvemos la lista de los planetas obligados que quedan por visitar en ese estado.
  

(defun f-search-state-equal-galaxy (node-1 node-2 &optional planets-mandatory)
  (cond 
   ((null planets-mandatory) (equal (node-state node-1) (node-state node-2)))
   (t (and (equal (node-state node-1) (node-state node-2)) 
           (equal (f-goal-test-galaxy-aux node-1 planets-mandatory) (f-goal-test-galaxy-aux node-2 planets-mandatory))))))
  
       
(f-search-state-equal-galaxy node-01 node-01) ;-> T
(f-search-state-equal-galaxy node-01 node-02) ;-> NIL
(f-search-state-equal-galaxy node-02 node-04) ;-> T

(f-search-state-equal-galaxy node-01 node-01 '(Avalon)) ;-> T
(f-search-state-equal-galaxy node-01 node-02 '(Avalon)) ;-> NIL
(f-search-state-equal-galaxy node-02 node-04 '(Avalon)) ;-> T

(f-search-state-equal-galaxy node-01 node-01 '(Avalon Katril)) ;-> T
(f-search-state-equal-galaxy node-01 node-02 '(Avalon Katril)) ;-> NIL
(f-search-state-equal-galaxy node-02 node-04 '(Avalon Katril)) ;-> NIL


;;
;; END: Exercise 3B  -- Equal predicate for search states
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     2. PROBLEM FORMALIZATION                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BEGIN: Exercise 4 -- Define the galaxy structure
;;
;;

(defparameter *galaxy-M35* 
  (make-problem 
   :states               *planets*          
   :initial-state        *planet-origin*
   :f-h                  #'(lambda (state) (f-h-galaxy state *sensors*))
   :f-goal-test          #'(lambda (node) (f-goal-test-galaxy node *planets-destination* *planets-mandatory*))
   :f-search-state-equal #'(lambda (node-1 node-2) (f-search-state-equal-galaxy node-1 node-2 *planets-mandatory*))
   :operators            (list 
                          #'(lambda (state)(navigate-white-hole state *white-holes*))
                          #'(lambda (state)(navigate-worm-hole state *worm-holes* *planets-forbidden*)))))


;;
;;  END: Exercise 4 -- Define the galaxy structure
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN Exercise 5: Expand node
;;
;; Returns wheter the goal is reached or not
;;
;; EXPAND-NODE
;;
;;  Input:
;;    node: expanded node
;;    problem: problem that we want resolve (galaxy-M35 in our case)
;;
;;  Returns:
;;    expanded nodes' list
;;

(defun expand-node (node problem)
  (if (funcall (problem-f-goal-test problem) node)
      'final
    (expand-node-aux node (build-actions node (problem-operators problem)) problem )))

(defun build-actions (node act)
  (if (null act)
      nil
    (append (funcall (first act) (node-state node)) (build-actions node (rest act)))))

(build-actions (make-node :state 'Kentares :depth 0 :g 0 :f 0) (problem-operators *galaxy-M35*))

(defun expand-node-aux (node list_actions problem)
  (let ((action (first list_actions)))
    (if (null list_actions)
        nil
      (let ((aux1 (action-final action))
            (aux2 (action-cost action)))
        (cons (make-node : state aux1
                         : parent node
                         : action action
                         : depth (+ 1 (node-depth node))
                         : g (+ (node-g node) aux2)
                         : h (funcall (problem-f-h problem) aux1)
                         : f (+ (+ (node-g node) aux2) (funcall (problem-f-h problem) aux1)))
              (expand-node-aux node (rest list_actions) problem))))))



(defparameter node-00
  (make-node :state 'Proserpina :depth 12 :g 10 :f 20) )

(defparameter lst-nodes-00
 (expand-node node-03 *galaxy-M35*))
(print lst-nodes-00)              

(expand-node (make-node :state 'Kentares :depth 0 :g 0 :f 0) *galaxy-M35*)
;;;(#S(NODE :STATE AVALON
;;;         :PARENT #S(NODE :STATE KENTARES
;;;                         :PARENT NIL
;;;                         :ACTION NIL
;;;                         :DEPTH 0
;;;                         :G ...)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE
;;;                           :ORIGIN KENTARES
;;;                           :FINAL AVALON
;;;                           :COST 3)
;;;         :DEPTH 1
;;;         :G ...)
;;; #S(NODE :STATE KATRIL
;;;         :PARENT #S(NODE :STATE KENTARES
;;;                         :PARENT NIL
;;;                         :ACTION NIL
;;;                         :DEPTH 0
;;;                         :G ...)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE
;;;                           :ORIGIN KENTARES
;;;                           :FINAL KATRIL
;;;                           :COST 10)
;;;         :DEPTH 1
;;;         :G ...)
;;; #S(NODE :STATE PROSERPINA
;;;         :PARENT #S(NODE :STATE KENTARES
;;;                         :PARENT NIL
;;;                         :ACTION NIL
;;;                         :DEPTH 0
;;;                         :G ...)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE
;;;                           :ORIGIN KENTARES
;;;                           :FINAL PROSERPINA
;;;                           :COST 7)
;;;         :DEPTH 1
;;;         :G ...)
;;; #S(NODE :STATE PROSERPINA
;;;         :PARENT #S(NODE :STATE KENTARES
;;;                         :PARENT NIL
;;;                         :ACTION NIL
;;;                         :DEPTH 0
;;;                         :G ...)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE
;;;                           :ORIGIN KENTARES
;;;                           :FINAL PROSERPINA
;;;                           :COST 12)
;;;         :DEPTH 1
;;;         :G ...))



;;
;; END Exercise 5: Expand node
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BEGIN Exercise 6 -- Node list management
;;
;;  Inserts a list of nodes into another list of nodes
;;
;; INSERT-NODES-STRATEGY
;;
;;  Input:
;;    node: one of our list of nodes
;;    lst-nodes: ordered list of nodes
;;    strategy: strategy used to put lists in order.
;;
;;  Returns:
;;    an ordered list with all nodes
;;

(defun node-g-<= (node-1 node-2)
  (<= (node-g node-1)
      (node-g node-2)))

(defparameter *uniform-cost*
  (make-strategy
   :name 'uniform-cost
   :node-compare-p #'node-g-<=))



(defun insert-nodes-strategy-aux (node lst-nodes strategy)
  (cond
   ((null lst-nodes) (list node))
   ((funcall (strategy-node-compare-p strategy) node (first lst-nodes))
    (cons node lst-nodes))
   (t 
    (cons (first lst-nodes) (insert-nodes-strategy-aux node (rest lst-nodes) strategy)))))

(defun insert-nodes-strategy (nodes lst-nodes strategy)
  (if (null nodes) 
      lst-nodes
    (insert-nodes-strategy (rest nodes) (insert-nodes-strategy-aux (first nodes) lst-nodes strategy) strategy)))


(defparameter node-01
   (make-node :state 'Avalon :depth 0 :g 0 :f 0) )
(defparameter node-02
  (make-node :state 'Kentares :depth 2 :g 50 :f 50) )

(print (insert-nodes-strategy (list node-00 node-01 node-02) 
                        lst-nodes-00 
                        *uniform-cost*));->
;;;
;;;(#S(NODE :STATE AVALON :PARENT NIL :ACTION NIL :DEPTH 0 :G 0 :H 0 :F 0)
;;; #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;; #S(NODE :STATE AVALON :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL AVALON :COST 12) :DEPTH 13 :G 22 :H 5 :F 27)
;;; #S(NODE :STATE DAVION :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL DAVION :COST 14) :DEPTH 13 :G 24 :H 1 :F 25)
;;; #S(NODE :STATE MALLORY :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 17) :DEPTH 13 :G 27 :H 7 :F 34)
;;; #S(NODE :STATE SIRTIS :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 10) :DEPTH 13 :G 20 :H 0 :F 20)
;;; #S(NODE :STATE KENTARES :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL KENTARES :COST 21) :DEPTH 13 :G 31 :H 4 :F 35)
;;; #S(NODE :STATE MALLORY :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 16) :DEPTH 13 :G 26 :H 7 :F 33)
;;; #S(NODE :STATE SIRTIS :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 7) :DEPTH 13 :G 17 :H 0 :F 17)
;;; #S(NODE :STATE KENTARES :PARENT NIL :ACTION NIL :DEPTH 2 :G 50 :H 0 :F 50)) 


(print 
 (insert-nodes-strategy (list node-00 node-01 node-02) 
                        (sort (copy-list lst-nodes-00) #'<= :key #'node-g) 
                        *uniform-cost*));->
;;;
;;;(#S(NODE :STATE AVALON :PARENT NIL :ACTION NIL :DEPTH 0 :G 0 :H 0 :F 0)
;;; #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;; #S(NODE :STATE SIRTIS :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 7) :DEPTH 13 :G 17 :H 0 :F 17)
;;; #S(NODE :STATE SIRTIS :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL SIRTIS :COST 10) :DEPTH 13 :G 20 :H 0 :F 20)
;;; #S(NODE :STATE AVALON :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL AVALON :COST 12) :DEPTH 13 :G 22 :H 5 :F 27)
;;; #S(NODE :STATE DAVION :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL DAVION :COST 14) :DEPTH 13 :G 24 :H 1 :F 25)
;;; #S(NODE :STATE MALLORY :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 16) :DEPTH 13 :G 26 :H 7 :F 33)
;;; #S(NODE :STATE MALLORY :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN PROSERPINA :FINAL MALLORY :COST 17) :DEPTH 13 :G 27 :H 7 :F 34)
;;; #S(NODE :STATE KENTARES :PARENT #S(NODE :STATE PROSERPINA :PARENT NIL :ACTION NIL :DEPTH 12 :G 10 :H 0 :F 20)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN PROSERPINA :FINAL KENTARES :COST 21) :DEPTH 13 :G 31 :H 4 :F 35)
;;; #S(NODE :STATE KENTARES :PARENT NIL :ACTION NIL :DEPTH 2 :G 50 :H 0 :F 50)) 


;;
;;    END: Exercize 6 -- Node list management
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             3. SEARCH                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 7 -- Definition of the A* strategy
;;
;; A strategy is, basically, a comparison function between nodes to tell 
;; us which nodes should be analyzed first. In the A* strategy, the first 
;; node to be analyzed is the one with the smallest value of g+h
;;

(defun node-f-<= (node-1 node-2)
  (<= (+ (node-g node-1) (node-h node-1))
      (+ (node-g node-2) (node-h node-2))))

(defparameter *A-star*
  (make-strategy 
   :name 'A-star
   :node-compare-p #'node-f-<=))

;;
;; END: Exercise 7 -- Definition of the A* strategy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    BEGIN Exercise 8: Search algorithm
;;
;; Do a search for the problem with a specifical strategy
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GRAPH-SEARCH
;;
;;  Input:
;;    problem:  the problem that we will try to resolve
;;    strategy: strategy used to resolve the problem
;;
;;  Returns:
;;    If there is no solution, returns NIL
;;    if not, a node that satisfied the goal test
;;

(defun graph-search (problem strategy)
  (let ((state (problem-initial-state problem)))
    (graph-search-aux problem strategy (list (make-node :state state
                                                       :parent nil
                                                       :action nil))
                      nil)))

(defun find-duplicates (node list problem)
  (let ((aux (first list)))
    (cond
     ((null list) nil)
     ((funcall (problem-f-search-state-equal problem) node aux) aux)
     (t (find-duplicates node (rest list) problem)))))

(defparameter node-03
   (make-node :state 'Mallory :depth 0 :g 0 :f 0) )
(find-duplicates node-03 (list node-03) *galaxy-M35*)


(defun graph-search-aux (problem strategy open-nodes closed-nodes)
  (unless (null open-nodes))
  (let ((actual (first open-nodes))
        (rep (find-duplicates (first open-nodes) closed-nodes problem)))
    (cond
     ((funcall (problem-f-goal-test problem) actual)
      actual)
     ((or (equal rep nil) (<= (node-g actual) (node-g rep)))
      (graph-search-aux problem 
                        strategy
                        (insert-nodes-strategy (expand-node actual problem) (rest open-nodes) strategy)
                        (cons actual closed-nodes)))
     (t (graph-search-aux problem
                         strategy
                         (rest open-nodes)
                         closed-nodes)))))
      
    
                                                   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A-STAR-SEARCH
;;
;;  Input:
;;    problem: the problem that we will try to resolve with A-STAR-SEARCH
;;
;;  Returns:
;;    If there is no solution, returns NIL
;;    if not, a node that satisfied the goal test
;;

(defun a-star-search (problem)
  (graph-search problem *A-star*))


(graph-search *galaxy-M35* *A-star*);->
;;;#S(NODE :STATE ...
;;;        :PARENT #S(NODE :STATE ...
;;;                        :PARENT #S(NODE :STATE ...)) 


(print (a-star-search *galaxy-M35*));->
;;;#S(NODE :STATE ...
;;;        :PARENT #S(NODE :STATE ...
;;;                        :PARENT #S(NODE :STATE ...)) 


;;; 
;;;    END Exercise 8: Search algorithm
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    BEGIN Exercise 9: Solution path / action sequence
;;
;; It shows the states visited and actions' sequence

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SOLUTION-PATH
;;
;;  Input:
;;    node: destination node
;;
;;  Returns:
;;    List of diferents planets that have been visited
;;

(defun solution-path (node)
  (if (null node)
      nil
    (append (solution-path (node-parent node)) (list (node-state node)))))

(solution-path nil) ;;; -> NIL 
(solution-path (a-star-search *galaxy-M35*))  ;;;-> (MALLORY ...)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACTION-SEQUENCE
;;
;;  Input:
;;    node: destination node
;;
;;  Returns:
;;    List of actions that have been done
;;

(defun action-sequence (node)
  (if (null node)
      nil
    (append (action-sequence (node-parent node)) (list (node-action node)))))

(action-sequence (a-star-search *galaxy-M35*))
;;; ->
;;;(#S(ACTION :NAME ...)) 

;; 
;;    END Exercise 9: Solution path / action sequence
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    BEGIN Exercise 10: depth-first / breadth-first
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         DEPTH-FIRST STRATEGY                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *depth-first*
  (make-strategy
   :name 'depth-first
   :node-compare-p #'depth-first-node-compare-p))

(defun depth-first-node-compare-p (node-1 node-2)
  (>= (node-depth node-1)
      (node-depth node-2)))

(defun depth-first-search (problem)
  (graph-search problem *depth-first*))

(solution-path (graph-search *galaxy-M35* *depth-first*))
;;; -> (MALLORY ... )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        BREADTH-SEARCH STRATEGY                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *breadth-first*
  (make-strategy
   :name 'breadth-first
   :node-compare-p #'breadth-first-node-compare-p))

(defun breadth-first-node-compare-p (node-1 node-2)
  (<= (node-depth node-1)
      (node-depth node-2)))

(defun breadth-first-search (problem)
  (graph-search problem *breadth-first*))

(solution-path (graph-search *galaxy-M35* *breadth-first*))
;; -> (MALLORY ... )

;;; 
;;;    END Exercise 10: depth-first / breadth-first
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
