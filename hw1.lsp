; Roxane Martin 
; 105327115

;; Question 1
;; argument: an integer n
;; returns: the nth Padovan number, where P(n) = P(n-2) + P(n-3)

(defun PAD (N) 
    (if (< N 3)
     1                                  ; base cases: returns 1 when N<3
    (+ (PAD (- N 2)) (PAD (- N 3)))     ; when N>=3, recursively calls PAD (n-2) and PAD (n-1) and add results 
    )
)


;; Question 2
;; argument: an integer n
;; returns: the number of additions required in computing PAD (n)

(defun SUMS (N) 
    (if (< N 3)
     0                                      ; base cases: returns 0 when N<3
    (+ 1 (SUMS (- N 2)) (SUMS (- N 3)))     ; when N>=3, recursively calls SUM (n-2) and SUM (n-1) and adds results + 1
    )
)


;; Question 3
;; argument: LISP representation of a tree 
;; returns: an anonymized tree with the same structure, where all symbols and numbers in the tree
;;          are replaced by a question mark.

(defun ANON (TREE)
    (cond ((null TREE) nil)                                 ;base case: empty tree, return nil
          ((atom TREE) '?)                                  ;base case: leaf node becomes ?
          (t (cons (ANON (car TREE)) (ANON (cdr TREE))))    ;else: recursively call ANON on (car TREE) and (cdr TREE) and "cons" results
    )
)