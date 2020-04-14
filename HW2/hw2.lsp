; BFS takes a TREE argument and returns a list of the leaves in breadth-first searched ordering,
; with the leaves shallowest appearing first in left to right ordering.
; The function effectively acts as a queue; if the first element of TREE is a list, it gets extended (dereferenced one level) to the end of TREE,
; then returns BFS on this new structure.
; If the first element of TREE is an atom, it becomes the first element of a returned list, where the remainder of the list is a recursive call
; to BFS with the rest of TREE as its argument.
(defun BFS (TREE)
  (cond
    ((or (atom TREE) (null TREE)) NIL)
    ((atom (first TREE)) (cons (first TREE) (BFS (rest TREE))))
    (t (BFS (append (rest TREE) (first TREE))))
  )
)

;; --------------------------------- ;;

; DFS takes a TREE argument and returns a list of the leaves in depth-first searched ordering, 
; with the leaves farthest to the right in the tree at the front of the returned list.
(defun DFS (TREE)
  (cond 
    ((null TREE) NIL)
    ((atom TREE) (list TREE))
    (t (append (DFS (rest TREE)) (DFS (first TREE))))
  )
)

;; ---- THE FOLLOWING HELPER FUNCTIONS PERTAIN TO QUESTION 3 RELATED TO THE DFID FUNCTION ---- ;;

; DFID-H returns a list of all the leaves in TREE from left to right at or above the depth DEPTH
(defun DFID-H (TREE DEPTH)
    (cond 
        ((null TREE) NIL)
        ((atom TREE) (list TREE))
        ((< DEPTH 1) NIL)
        (t (append (DFID-H (first TREE) (1- DEPTH)) (DFID-H (rest TREE) DEPTH)))
    )
)

; DFID-C, or DFID-(counter), applies DFID-H iteratively between the arguments CURR-D (current-depth) and MAX-D (max-depth)
(defun DFID-C (TREE CURR-D MAX-D)
    (cond
        ((> CURR-D MAX-D) NIL)
        (t (append (DFID-H TREE CURR-D) (DFID-C TREE (1+ CURR-D) MAX-D)))
    )
)

; DFID takes a tree argument TREE and an integer DEPTH and returns a depth-first iterative deepening searched list
; of the leaves in the tree. It iteratively prints the leaves appearing at or above the depths between 0 and DEPTH.
(defun DFID (TREE DEPTH)
    (DFID-C TREE 0 DEPTH)
)

;; --------------------------------- ;;

; These functions implement a depth-first solver for the missionary-cannibal
; problem. In this problem, three missionaries and three cannibals are trying to
; go from the east side of a river to the west side. They have a single boat
; that can carry two people at a time from one side of the river to the
; other. There must be at least one person in the boat to cross the river. There
; can never be more cannibals on one side of the river than missionaries. If
; there are, the cannibals eat the missionaries.

; In this implementation, a state is represented by a single list
; (missionaries cannibals side). side represents which side the boat is
; currently on, and is T if it is on the east side and NIL if on the west
; side. missionaries and cannibals represent the number of missionaries and
; cannibals on the same side as the boat. Thus, the initial state for this
; problem is (3 3 T) (three missionaries, three cannibals, and the boat are all
; on the east side of the river) and the goal state is (3 3 NIL).

; The main entry point for this solver is the function MC-DFS, which is called
; with the initial state to search from and the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, MC-DFS returns NIL.

; To call MC-DFS to solve the original problem, one would call (MC-DFS '(3 3 T)
; NIL) -- however, it would be possible to call MC-DFS with a different initial
; state or with an initial path.

; Examples of calls to some of the helper functions can be found after the code.


; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
  (equal s '(3 3 NIL))
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; missionaries to move (m), and a number of cannibals to move (c). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
  (let ((miss (+ m (- 3 (first s)))) (cann (+ c (- 3 (second s)))))
    (if (or (> cann miss 0) (< cann miss 3)) 
      NIL
      (list (list miss cann (not (third s))))
    )  
  )
)

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun succ-fn (s)
  (append 
    (if (> (first s) 0) (next-state s 1 0) NIL)
    (if (> (first s) 1) (next-state s 2 0) NIL)
    (if (> (second s) 0) (next-state s 0 1) NIL)
    (if (> (second s) 1) (next-state s 0 2) NIL)
    (if (and (> (first s) 0) (> (second s) 0)) (next-state s 1 1) NIL)
  )
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.
(defun on-path (s states)
  (cond
    ((null states) NIL)
    ((equal s (first states)) T)
    (t (on-path s (rest states)))
  )
)

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states from the current state (states).
; MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL. 
; Note that the path should be ordered as: (S_n ... S_2 S_1 S_0)
(defun mult-dfs (states path)
  (cond
    ((null states) NIL)
    ((mc-dfs (first states) path) (mc-dfs (first states) path))
    (t (mult-dfs (rest states) path))
  )
)

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun mc-dfs (s path)
  (cond 
    ((final-state s) (cons s path))
    ((on-path s path) NIL)
    (t (mult-dfs (succ-fn s) (cons s path)))
  )
)
