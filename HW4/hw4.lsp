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
    (backtrace n () delta ())
)

(defun dfs (n delta assign cands)
    ;; (print assign)
    ;; (print delta)
    ;; (print cands)
    ;; (print "-----")
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
    ;; (print delta)
    ;; (print value)
    ;; (print "-----")
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
        ((null (rest clause)) (if (= (first clause) value) NIL 0))
        (t (new-delta-clause clause value))
    )
)

(defun new-delta-clause (clause value)
    (cond ((null clause) NIL)
        ((= value (first clause)) NIL)
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








(defun sat-dfs (n csp assignment)
    (print assignment)
    (let* ((posvalue (get-next-var assignment)) (negvalue (* -1 posvalue)))
        (cond ((= n (length assignment)) assignment)
            ((and (if-consistent posvalue posvalue csp) 
                (sat-dfs n csp (cons posvalue assignment))
            ) 
                    (sat-dfs n csp (cons posvalue assignment)))
            ((and (if-consistent posvalue negvalue csp) 
                (sat-dfs n csp (cons negvalue assignment))
            ) 
                    (sat-dfs n csp (cons negvalue assignment)))
            (t NIL)
        )
    )
)

(defun remove-list (L list)
    (cond ((null L) NIL)
        ((equal (first L) list) (remove-list (rest L) list))
        (t (cleanUpList (cons (first L) (remove-list (rest L) list)))) 
    )
)

(defun L-in-list (L list)
    (cond ((null list) NIL)
        ((equal (first list) L) t)
        (t (L-in-list L (rest list)))
    )
)

(defun remove-solved (csp solved)
    (cond ((null csp) solved)
        ((L-in-list (first csp) solved) (remove-solved (rest csp) solved))
    )
)

(defun solved-clauses (val csp &optional (solved NIL))
    (cond ((null csp) solved)
        (t (solved-clauses val (rest csp) (cons (solved-clause val (first csp)) solved)))
    )
)

(defun solved-clause (val clause &optional (i 0))
    (cond ((>= i (length clause)) NIL)
        ((= val (nth i clause)) clause)
        (t (solved-clause val clause (1+ i)))
    )
)

; returns a list of all clauses in csp that assigning val to var then solves
(defun if-consistent (var val csp)
    (cond ((null csp) t)
        ((not (if-consistent-clause var val (first csp) NIL)) NIL)
        (t (if-consistent var val (rest csp)))
    )
)

; determines if clause is still consistent when making var->val.
; remaining-literal should be NIL at first, becomes t if a literal exists
; that hasn't been assigned yet (abs greater than abs of var)
(defun if-consistent-clause (var val clause remaining-literal)
    (cond ((null clause) remaining-literal)
        ((= val (first clause)) t)
        (t 
            (if-consistent-clause
                var val (rest clause) 
                    (cond (remaining-literal t)
                        (t (> (abs (first clause)) var))
                    )    
            )
        )
    )
)

(defun get-next-var (assignment)
    (1+ (length assignment))
)