(defun memb (x L)
    (cond 
    ((null L) NIL)
    ((equal x (car L)) t)
    (t (memb x (cdr L)))
    )
)

(defun final (L)
    (cond
        ((null (cdr L)) (car L))
        (t (final (cdr L)))
    )
)

(defun n-th (l n)
    (cond
        ((= n 0) (first l))
        (t (n-th (rest l) (- n 1)))
    )
)

(defun remv (elm list)
    (cond
        ((null list) nil)
        ((equal (first list) elm) (remv elm (rest list)))
        (t (cons (first list) (remv elm (rest list))))
    )
)

(defun appd (l1 l2)
    (cond
        ((null l1) 
            l2)
        (t 
            (cons (first l1) (appd (rest l1) l2)))
    )
)

; (print (appd '(a b) '(a b c)))
; (print (n-th '(a b) 0))
; (print (last ()))
; (print (memb 'c '(a b d c f)))

(defun foo (a)
    (setq x 3)
    (+ a x)
)

(setq x 5)
(foo 3)
(print x)