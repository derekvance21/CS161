; NAME: Derek Vance
; EMAIL: dvance@g.ucla.edu
; hw4.lsp

(defun cleanUpList (L)
    (cond ((null L) nil)
        ((equal L 0) L)
        (t (let ((cur (car L))
                (res (cleanUpList (cdr L)))
                )
                (if cur 
                (cons cur res)
                res
                )
            );end let
        );end t
    );end cond
);end 

(defun sat? (n delta)
    (populate-solution n (dfs n delta NIL '(1 -1)))
)

(defun populate-solution (n solution)
    (cond
        ((null solution) NIL)
        ((>= (abs (first solution)) n) solution)
        (t (populate-solution n (cons (1+ (abs (first solution))) solution)))
    )
)

(defun dfs (n delta assign cands)
    (cond ((equal 0 delta) NIL)
        ((null delta) assign)
        ((>= (length assign) n) NIL)
        ((null cands) NIL)
        ((null delta) assign)
        (t (let ((sol (dfs n (cleanUpList (new-delta delta (first cands))) (cons (first cands) assign) (new-cands cands))))
                (if (not (null sol)) sol (dfs n delta assign (rest cands)))
            )
        )
    )
)

(defun new-delta (delta value)
    (cond ((null delta) NIL)
        ((equal 0 (delta-clause (first delta) value)) 0)
        (t (let ((remaining-delta (new-delta (rest delta) value)))
                (if (equal remaining-delta 0)
                    0
                    (cons (delta-clause (first delta) value) remaining-delta)
                )
            )
        )
    )
)

(defun delta-clause (clause value)
    (cond 
        ((and (null (rest clause)) (= (abs (first clause)) (abs value))) (if (= (first clause) value) NIL 0))
        ((value-in-clause clause value) NIL)
        (t (new-delta-clause clause value))
    )
)

(defun value-in-clause (clause value)
    (cond ((null clause) NIL)
        ((= (first clause) value) t)
        (t (value-in-clause (rest clause) value))
    )
)

(defun new-delta-clause (clause value)
    (cond ((null clause) NIL)
        (t (let ((literal (new-delta-clause (rest clause) value)))
                (if (equal literal 0) 
                    0 
                    (if (= (abs value) (abs (first clause))) 
                        literal
                        (cons (first clause) literal)
                    )
                )
            )
        )
    )
)

(defun new-cands (cands)
    (list 
        (1+ (abs (first cands)))
        (1- (* -1 (abs (first cands))))
    )
)





