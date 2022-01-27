;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

(defun BFS (FRINGE)
    (cond
    ((null FRINGE) nil)                                                 ;If given FRINGE is empty, return nil
    ((atom (car FRINGE)) (cons (car FRINGE) (BFS (cdr FRINGE))) )       ;If current node (i.e. first item in FRINGE) is an atom, 
                                                                        ; add it to "visted nodes" list
                                                                        ; and recurse on rest of FRINGE
    (t (BFS (append (cdr FRINGE) (car FRINGE))))                        ;Else if current node is a list, 
                                                                        ; add contents of current node to end of FRINGE 
                                                                        ; (i.e. what is being passed to recursive BFS call)
    ) 
)


;(print (BFS '(ROOT)))
;(print (BFS '((((L E) F) T))))
;(print (BFS '((R (I (G (H T)))))))
;(print (BFS '(((A (B)) C (D)))))
;(print (BFS '((T (H R E) E))))
;(print (BFS '((A ((C ((E) D)) B)))))

;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;


; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.
(defun FINAL-STATE (S)
    (if (equal S '(T T T T))    ;If S equals (T T T T), 
     T)                         ;return T
)                               ;Otherwise, do nothing (returns NIL)



; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
(defun NEXT-STATE (S A)
    (let ((homer (first S)) (baby (second S)) (dog (third S)) (poison (fourth S)))  ;Let items in list S be (homer baby dog poison)
    (cond   ((equal A 'h)                                                           ;If current move is homer
                (cond
                    ((or (equal baby dog) (equal baby poison)) nil)                 ;If baby&dog left on same side or baby&poison left on same side, return nil 
                    (t (list (list (not homer) baby dog poison)))                   ;Else return ((not homer) (rest of list))
                )
            )
            ((equal A 'b)                                                           ;If current move is baby and homer
                (cond 
                    ((not (equal homer baby)) nil)                                  ;If baby&homer are not on same side, return nil 
                    (t (list (list (not homer) (not baby) dog poison)))             ;Else return ((not homer) (not baby) (rest of list))
                )
            )
            ((equal A 'd)                                                           ;If current move is dog and homer
                (cond
                    ((not (equal homer dog)) nil)                                   ;If dog&homer are not on same side, return nil       
                    ((equal baby poison) nil)                                       ;If baby&poison left on same side, return nil 
                    (t (list (list (not homer) baby (not dog) poison)))             ;Else return ((not homer) baby (not dog) poison)
                )
            )
            ((equal A 'p)                                                           ;If current move is poison and homer
                (cond
                    ((not (equal homer poison)) nil)                                ;If poison&homer are not on same side, return nil
                    ((equal baby dog) nil)                                          ;If baby&dog left on same side, return nil
                    (t (list (list (not homer) baby dog (not poison))))             ;Else return ((not homer) baby dog (not poison))
                )
            )
            (t nil)                                                                 ;If input is anything else, return nil
    )
    )
)


; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun SUCC-FN (S)
    (loop for x in '(h b d p)           ;For possible moves (h b d p)
        append (NEXT-STATE S x)         ;Collect all next states
    )
)


; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.
(defun ON-PATH (S STATES)
    (cond   ((null STATES) nil)                     ;If STATES is empty, return nil
            ((equal (car STATES) S) T)              ;If first item in STATES equals S, return t
            (t (ON-PATH S (cdr STATES)) )           ;Else, recurse on rest of STATES
    )            
)


; MULT-DFS takes two arguments: a list of states from the initial state to the
; current state (PATH), and the legal successor states to the last, current 
; state in the PATH (STATES). MULT-DFS does a depth-first search on each element 
; of STATES in turn. If any of those searches reaches the final state, MULT-DFS 
; returns the complete path from the initial state to the goal state. 
; Otherwise, it returns NIL.
(defun MULT-DFS (STATES PATH)                                                   
    (cond   ((null STATES) nil)                                                 ;If STATES is empty, return nil
            ((null (DFS (car STATES) PATH)) (MULT-DFS (cdr STATES) PATH))       ;If there no path to goal from current state, recurse on rest of STATES
            (t (DFS (car STATES) PATH))                                         ;Else, return path from current state to goal
    )
)


; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). DFS performs a depth-first search starting at the given state. 
; It returns the path from the initial state to the goal state, if any, 
; or NIL otherwise. 
(defun DFS (S PATH)
    (cond   ((FINAL-STATE S) (append PATH (list S)))                            ;If S is goal, return current path + S
            ((ON-PATH S PATH) nil)                                              ;If S in already in path, return nil
            (t (MULT-DFS (SUCC-FN S) (append PATH (list S))))                   ;Else, call MULT-DFS with successors of S and current path + S
    )
)
    
; (print (DFS '(NIL NIL NIL NIL) NIL))
; output: 
; ((NIL NIL NIL NIL)    (T T NIL NIL) 
; (NIL T NIL NIL)       (T T T NIL) 
; (NIL NIL T NIL)       (T NIL T T) 
; (NIL NIL T T)         (T T T T)) 