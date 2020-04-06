(defun TEST (a b)
    (if (equal a b)
        (print 'passed)
        (print 'failed)
    )
)


(defun TREE-CONTAINS (N TREE)
    (cond
        ((null TREE) NIL)
        ((equal N TREE) t)
        ((atom TREE) NIL)
        ((= N (second TREE)) t)
        ((> N (second TREE)) (TREE-CONTAINS N (third TREE)))
        ((< N (second TREE)) (TREE-CONTAINS N (first TREE)))
    )
)


(defun TREE-MIN (TREE)
    (cond
        ((numberp TREE) TREE)
        (t (TREE-MIN (first TREE)))
    )
)


(defun TREE-ORDER (TREE)
    (cond 
        ((numberp TREE) (list TREE))
        (t (append (list (second TREE)) (TREE-ORDER (first TREE)) (TREE-ORDER (third TREE))))
    )
)


(defun SUB-LIST (L START LEN)
    (cond
        ((= LEN 0) ())
        ((= START 0) (cons (first L) (SUB-LIST (rest L) START (1- LEN))))
        (t (SUB-LIST (rest L) (1- START) LEN))
    )
)


(defun SPLIT-LIST (L)
    (list 
        (SUB-LIST L 0 (if (oddp (length L)) (/ (+ 1 (length L)) 2) (/ (length L) 2)))
        (SUB-LIST L (if (oddp (length L)) (/ (+ 1 (length L)) 2) (/ (length L) 2)) (if (oddp (length L)) (/ (1- (length L)) 2) (/ (length L) 2)))
    )
)


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


(defun LIST2BTREE (LEAVES) 
    (cond
        ((null (cdr LEAVES)) (first LEAVES))
        (t (list (LIST2BTREE (first (SPLIT-LIST LEAVES))) (LIST2BTREE (second (SPLIT-LIST lEAVES)))))
    )
)


(defun BTREE2LIST (TREE)
    (cond
        ((atom TREE) (list TREE))
        (t (append (BTREE2LIST (first TREE)) (BTREE2LIST (second TREE))))
    )
)


(defun IS-SAME (E1 E2)
    (cond
        ((and (not E1) (not E2)) T)
        ((or (not E1) (not E2)) NIL)
        ((and (numberp E1) (numberp E2)) (= E1 E2))
        ((or (numberp E1) (numberp E2)) NIL)
        ((IS-SAME (first E1) (first E2)) (IS-SAME (rest E1) (rest E2)))
    )
)

; cons ~ python append, append ~ python extend, list = new list with args as elements of that list
; Validation
; 1)
(print '1)
(TEST (TREE-CONTAINS 3 '((1 2 3) 7 8)) T)
(TEST (TREE-CONTAINS 4 '((1 2 3) 7 8)) NIL)

; 2)
(print '2)
(TEST (TREE-MIN '((1 2 3) 7 8)) 1)

; 3)
(print '3)
(TEST (TREE-ORDER 3) '(3))
(TEST (TREE-ORDER '((1 2 3) 7 8)) '(7 2 1 3 8))

; 4)
(print '4)
(TEST (SUB-LIST '(a b c d) 0 3) '(a b c))
(TEST (SUB-LIST '(a b c d) 3 1) '(d))
(TEST (SUB-LIST '(a b c d) 2 0) NIL)

; 5) you can call SUB-LIST from SPLIT-LIST
(print '5)
(TEST (SPLIT-LIST '(a b c d)) '((a b) (c d)))
(TEST (SPLIT-LIST '(a b c d e)) '((a b c) (d e))) ; ((a b) (c d e)) is incorrect
(TEST (SPLIT-LIST '(a b c d e f)) '((a b c) (d e f)))

; 6)
(print '6)
(TEST (BTREE-HEIGHT 1) 0)
(TEST (BTREE-HEIGHT '(1 2)) 1)
(TEST (BTREE-HEIGHT '(1 (2 3))) 2)
(TEST (BTREE-HEIGHT '((1 2) (3 4))) 2)
(TEST (BTREE-HEIGHT '((1 (2 3)) ((4 5) (6 7)))) 3)
(TEST (BTREE-HEIGHT '(((1 2) (3 4)) ((5 6) (7 8)))) 3)

; 7) you can call SPLIT-LIST from LIST2BTREE
(print '7)
(TEST (LIST2BTREE '(1)) 1)
(TEST (LIST2BTREE '(1 2)) '(1 2))
(TEST (LIST2BTREE '(1 2 3)) '((1 2) 3))
(TEST (LIST2BTREE '(1 2 3 4)) '((1 2) (3 4)))
(TEST (LIST2BTREE '(1 2 3 4 5 6 7)) '(((1 2) (3 4)) ((5 6) 7)))
(TEST (LIST2BTREE '(1 2 3 4 5 6 7 8)) '(((1 2) (3 4)) ((5 6) (7 8))))

; 8) (BTREE2LIST (LIST2BTREE X)) == X for all lists of atoms X
(print '8)
(TEST (BTREE2LIST 1) '(1))
(TEST (BTREE2LIST '(1 2)) '(1 2))
(TEST (BTREE2LIST '((1 2) 3)) '(1 2 3))
(TEST (BTREE2LIST '((1 2) (3 4))) '(1 2 3 4))
(TEST (BTREE2LIST '(((1 2) (3 4)) ((5 6) 7))) '(1 2 3 4 5 6 7))
(TEST (BTREE2LIST '(((1 2) (3 4)) ((5 6) (7 8)))) '(1 2 3 4 5 6 7 8))

; 9)
(print '9)
(TEST (IS-SAME '((1 2 3) 7 8) '((1 2 3) 7 8)) T)
(TEST (IS-SAME '(1 2 3 7 8) '((1 2 3) 7 8)) NIL)

