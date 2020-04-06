; TREE-CONTAINS takes two arguments, a number N and an ordered tree TREE, and returns a boolean if N appears in TREE
; If TREE is not a list, the function returns if TREE == N.
; If TREE is a list, the function compares N to the second element, m, of TREE and recursively calls itself 
; with either the first or third element of TREE if N was less than or greater than m, respectively, or returns T if N == m.
(defun TREE-CONTAINS (N TREE)
    (cond
        ((null TREE) NIL)
        ((not (listp TREE)) (= N TREE))
        ((= N (second TREE)) t)
        ((> N (second TREE)) (TREE-CONTAINS N (third TREE)))
        ((< N (second TREE)) (TREE-CONTAINS N (first TREE)))
    )
)

; TREE-MIN takes an ordered tree TREE as its one argument, and returns the minimum of the ordered tree
; If TREE is a number and not a list, return it.
; If TREE is a list, recursively call itself with the first element of TREE, thus progressing to the minimum of TREE
(defun TREE-MIN (TREE)
    (cond
        ((numberp TREE) TREE)
        (t (TREE-MIN (first TREE)))
    )
)

; TREE-ORDER takes an ordered tree TREE as its one argument, and returns a pre-ordered list of the numbers in TREE
; if TREE is a number and not a list, return TREE as a one element list.
; if TREE is a list, return a list of the TREE-ORDER'ed second, first, and third elements of TREE.
(defun TREE-ORDER (TREE)
    (cond 
        ((numberp TREE) (list TREE))
        (t (append (TREE-ORDER (second TREE)) (TREE-ORDER (first TREE)) (TREE-ORDER (third TREE))))
    )
)

; SUB-LIST takes a list L and two non-negative integers START and LEN, and returns a sublist of length LEN starting at START. 
; ie. L[START:START+LEN]
; if LEN is 0, return the empty list.
; if START is 0, return a list starting with the first element of L and the rest as a recursive call of SUB-LIST, with LEN decremented by 1.
; if neither of above, return a recursive call of SUB-LIST, with the "rest" of L, START decremented by 1, and LEN as is.
(defun SUB-LIST (L START LEN)
    (cond
        ((= LEN 0) ())
        ((= START 0) (cons (first L) (SUB-LIST (rest L) START (- LEN 1))))
        (t (SUB-LIST (rest L) (- START 1) LEN))
    )
)

; SPLIT-LIST takes a list L as its one argument and returns a list of two lists L1 and L2 in that order,
; where L1's length is the same or greater by 1 of L2's length
; returns a list where the first element is SUB-LIST with L, START as 0, and LEN as half the length of L rounded up,
; and the second element is SUB-LIST with L, START as half the length of L rounded up, and LEN as half the length of L rounded down
(defun SPLIT-LIST (L)
    (list 
        (SUB-LIST L 0 (if (oddp (length L)) (/ (+ 1 (length L)) 2) (/ (length L) 2)))
        (SUB-LIST L (if (oddp (length L)) (/ (+ 1 (length L)) 2) (/ (length L) 2)) (if (oddp (length L)) (/ (1- (length L)) 2) (/ (length L) 2)))
    )
)

; BTREE-HEIGHT takes a binary tree TREE as its one argument and returns the height of TREE.
; If TREE is not a list or a one element list, return 0.
; Otherwise, recursively call BTREE-HEIGHT on the first and second elements of TREE (adding 1), and return the max of those two.
(defun BTREE-HEIGHT (TREE)
    (cond
        ((or (atom TREE) (null (rest TREE))) 0)
        (t 
            (let ((heightL (+ 1 (BTREE-HEIGHT (first TREE)))) (heightR (+ 1 (BTREE-HEIGHT (second TREE)))))
                (if (> heightL heightR)
                    heightL
                    heightR
                )
            )
        )
    )
)

; LIST2BTREE takes a non-empty list of atoms LEAVES as its one argument, and returns a binary tree,
; where every internal node's left child subtree has either the same of one more than the number of leaves as the node's right child subtree 
; if LEAVES is a one element list, return the first element.
; Otherwise, return a list where the first and second elements are the recursive calls of LIST2BTREE with the 
; first and second elements of (SPLIT-LIST LEAVES)
(defun LIST2BTREE (LEAVES) 
    (cond
        ((null (cdr LEAVES)) (first LEAVES))
        (t (list (LIST2BTREE (first (SPLIT-LIST LEAVES))) (LIST2BTREE (second (SPLIT-LIST lEAVES)))))
    )
)

; BTREE2LIST takes a binary tree TREE as its one argument and returns a list of atoms that appear in TREE
; if TREE is an atom, return TREE as a list
; otherwise, return a list where the elements are the recursive calls of BTREE2LIST with the first and second elements of TREE as inputs
(defun BTREE2LIST (TREE)
    (cond
        ((atom TREE) (list TREE))
        (t (append (BTREE2LIST (first TREE)) (BTREE2LIST (second TREE))))
    )
)

; IS-SAME takes two LISP expressions E1 and E2 where all atoms are numbers and checks whether the expressions are equal
; if E1 and E2 are empty lists, return T
; if one but not the other is an empty list, return NIL
; if both are numbers, return (= E1 E2)
; if one but no the other is a number, return NIL
; if the first elements of E1 and E2 are same (using a recursive call of IS-SAME), return IS-SAME with the rests of E1 and E2 as inputs
; otherwise, return NIL (line not needed but good for readability as cond returns NIL if no conditions are met)
(defun IS-SAME (E1 E2)
    (cond
        ((and (not E1) (not E2)) T)
        ((or (not E1) (not E2)) NIL)
        ((and (numberp E1) (numberp E2)) (= E1 E2))
        ((or (numberp E1) (numberp E2)) NIL)
        ((IS-SAME (first E1) (first E2)) (IS-SAME (rest E1) (rest E2)))
        (t NIL)
    )
)


;; ; VALIDATION - defined a function TEST that prints passed if the arguments are equal and failed if not
;; (defun TEST (a b)
;;     (if (equal a b)
;;         (print 'passed)
;;         (print 'failed)
;;     )
;; )

;; ; Here are the test cases I used, taken from the homework specification:

;; (print '1)
;; (TEST (TREE-CONTAINS 3 '((1 2 3) 7 8)) T)
;; (TEST (TREE-CONTAINS 4 '((1 2 3) 7 8)) NIL)

;; (print '2)
;; (TEST (TREE-MIN '((1 2 3) 7 8)) 1)

;; (print '3)
;; (TEST (TREE-ORDER 3) '(3))
;; (TEST (TREE-ORDER '((1 2 3) 7 8)) '(7 2 1 3 8))

;; (print '4)
;; (TEST (SUB-LIST '(a b c d) 0 3) '(a b c))
;; (TEST (SUB-LIST '(a b c d) 3 1) '(d))
;; (TEST (SUB-LIST '(a b c d) 2 0) NIL)

;; (print '5)
;; (TEST (SPLIT-LIST '(a b c d)) '((a b) (c d)))
;; (TEST (SPLIT-LIST '(a b c d e)) '((a b c) (d e))) ; ((a b) (c d e)) is incorrect
;; (TEST (SPLIT-LIST '(a b c d e f)) '((a b c) (d e f)))

;; (print '6)
;; (TEST (BTREE-HEIGHT 1) 0)
;; (TEST (BTREE-HEIGHT '(1 2)) 1)
;; (TEST (BTREE-HEIGHT '(1 (2 3))) 2)
;; (TEST (BTREE-HEIGHT '((1 2) (3 4))) 2)
;; (TEST (BTREE-HEIGHT '((1 (2 3)) ((4 5) (6 7)))) 3)
;; (TEST (BTREE-HEIGHT '(((1 2) (3 4)) ((5 6) (7 8)))) 3)

;; (print '7)
;; (TEST (LIST2BTREE '(1)) 1)
;; (TEST (LIST2BTREE '(1 2)) '(1 2))
;; (TEST (LIST2BTREE '(1 2 3)) '((1 2) 3))
;; (TEST (LIST2BTREE '(1 2 3 4)) '((1 2) (3 4)))
;; (TEST (LIST2BTREE '(1 2 3 4 5 6 7)) '(((1 2) (3 4)) ((5 6) 7)))
;; (TEST (LIST2BTREE '(1 2 3 4 5 6 7 8)) '(((1 2) (3 4)) ((5 6) (7 8))))

;; (print '8)
;; (TEST (BTREE2LIST 1) '(1))
;; (TEST (BTREE2LIST '(1 2)) '(1 2))
;; (TEST (BTREE2LIST '((1 2) 3)) '(1 2 3))
;; (TEST (BTREE2LIST '((1 2) (3 4))) '(1 2 3 4))
;; (TEST (BTREE2LIST '(((1 2) (3 4)) ((5 6) 7))) '(1 2 3 4 5 6 7))
;; (TEST (BTREE2LIST '(((1 2) (3 4)) ((5 6) (7 8)))) '(1 2 3 4 5 6 7 8))

;; (print '9)
;; (TEST (IS-SAME '((1 2 3) 7 8) '((1 2 3) 7 8)) T)
;; (TEST (IS-SAME '(1 2 3 7 8) '((1 2 3) 7 8)) NIL)
