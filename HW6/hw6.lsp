; EXERCISE: Fill this function.
; returns the index of the variable
; that corresponds to the fact that 
; "node n gets color c" (when there are k possible colors).
;
(defun node2var (n c k)
    (+ c (* k (- n 1)))
)

; EXERCISE: Fill this function
; returns *a clause* for the constraint:
; "node n gets at least one color from the set {c,c+1,...,k}."
;
(defun at-least-one-color (n c k)
    (cond 
        ((= c k) (list (node2var n c k)))
        (t (append (list (node2var n c k)) (at-least-one-color n (1+ c) k)))
    )
)

; EXERCISE: Fill this function
; returns *a list of clauses* for the constraint:
; "node n gets at most one color from the set {c,c+1,...,k}."
;
(defun at-most-one-color (n c k)
    (defun at-most-h (n c k u) ; returns list of clauses: (-c, -c-1), (-c, -c-2), ..., (-c, -k) [all node2var'd first tho]
        (cond
            ((> u k) NIL)
            (t (cons (list (* -1 (node2var n c k)) (* -1 (node2var n u k))) (at-most-h n c k (1+ u))))
        )
    )
    (cond
        ((= c k) NIL)
        (t (append (at-most-h n c k (1+ c)) (at-most-one-color n (1+ c) k)))
    )
)

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "node n gets exactly one color from the set {1,2,...,k}."
;
(defun generate-node-clauses (n k)
    (cons (at-least-one-color n 1 k) (at-most-one-color n 1 k))
)

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "the nodes at both ends of edge e cannot have the same color from the set {1,2,...,k}."
;
; use at-least-one-color n c k
(defun generate-edge-clauses (e k)
    (defun gen-h (e c k) ; prohibits nodes of e to both be colored colors from c to k
        (cond
            ((= c k) (list (list (* -1 (node2var (first e) c k)) (* -1 (node2var (second e) c k)))))
            (t (cons (list (* -1 (node2var (first e) c k)) (* -1 (node2var (second e) c k))) (gen-h e (1+ c) k)))
        )
    )
    (gen-h e 1 k)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Your exercises end here. Below are top-level
; and utility functions that you do not need to understand.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
