(defun TEST(a b)
  (print a)
  (print b)
  (if (equal a b)
    (print 'passed)
    (print 'failed)
  )
)

;;  1. Write a single pure LISP function, called BFS, that performs a breadth-first search of a tree.  The function should take a single argument that is the list representation of the tree, and returns a single, top-level list of the terminal nodes in the order they would be visited by a left-to-right breadth-first search. For example, (bfs '((A (B)) C (D))) would return (C A D B). Do not use any auxiliary functions. 

(defun BFS (TREE)
  (cond 
    ((null TREE) NIL)
    ((atom TREE) (list TREE))
    (t ())
  )
)

(TEST (BFS '((A (B)) C (D))) '(C A D B))

;;  2. Write a single pure LISP function, called DFS, that performs a depth-first search of a tree.  The function should take a single argument that is the list representation of the tree, and returns a single, top-level list of the terminal nodes in the order they would be visited by a right-to-left depth-first search.  For example, (dfs '((A (B)) C (D))) would return (D C B A). Do not use any auxiliary functions. 

(defun DFS (TREE)
  (cond 
    ((null TREE) NIL)
    ((atom TREE) (list TREE))
    (t (append (DFS (rest TREE)) (DFS (first TREE))))
  )
)

(TEST (DFS '((A (B)) C (D))) '(D C B A))

;; 3. Write a set of pure LISP functions that implement depth-first iterative-deepening. Your top-level function, called DFID, should take two arguments, the list representation of the tree, and an integer representing the maximum depth of the tree, and returns a single top-level list of the terminal nodes in the order that they would be visited by a left-to-right depth-first iterative-deepening search. Note that those nodes that are visited in multiple iterations will appear multiple times in the output list. For example, (dfid '((A (B)) C (D)) 3) would return (C A C D A B C D). 

(defun DFID-H (TREE DEPTH)
    (cond 
        ((null TREE) NIL)
        ((atom TREE) (list TREE))
        ((< DEPTH 1) NIL)
        (t (append (DFID-H (first TREE) (1- DEPTH)) (DFID-H (rest TREE) DEPTH)))
    )
)

(defun DFID-C (TREE CURR-D MAX-D)
    (cond
        ((> CURR-D MAX-D) NIL)
        (t (append (DFID-H TREE CURR-D) (DFID-C TREE (1+ CURR-D) MAX-D)))
    )
)

(defun DFID (TREE DEPTH)
    (DFID-C TREE 0 DEPTH)
)

(print "testing DFID")
(print (DFID '((a (b)) c (d)) 3))

;; Each of these functions must work for trees of arbitrary depth and branching factor, and hence you may not assume any a priori upper bound on these parameters. Be sure to exhibit sufficient test cases to convince yourself and us that your programs work in general. Try at least the examples above, as well as the list (A (B C) (D) (E (F G))). 
 
;; 4. Implement a depth-first solver for the missionary-cannibal problem that was described in class. To implement this solver, we provide you with a code skeleton (hw2_skeleton.lsp). Implement the functions in the code skeleton as described in their associated comments. DO NOT CHANGE THE FUNCTION NAMES OR PARAMETERS. Also do not write any additional functions. 

