;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (r c).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list row x)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
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

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.

;

; function returns T if the row doesn't contain any boxes (2) or keepers (3). Else returns NIL
(defun goal-row (row)
	(cond ((null row) T) ; at end of row, no boxes or keepers found
		((or (= 2 (first row)) (= 3 (first row))) NIL)
		(t (goal-row (rest row)))
	)
)

; function returns T if the game state s is finished, i.e. no 2's or 3's left on the board
(defun goal-test (s)
  	(cond ((null s) T) ; at end of state rows, no boxes or keepers found
		((not (goal-row (first s))) NIL)
		(t (goal-test (rest s)))
	)
);end defun

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;

; function returns the number of elements in a list
(defun sizeof (LIST &optional (size 0))
	(cond ((null LIST) size)
		(t (sizeof (rest LIST) (1+ size)))
	)
)

; function returns the value of the square at row r and column c in game state s
(defun get-square (s r c)
	(cond
		((or (< r 0) (< c 0) (>= r (sizeof s)) (>= c (sizeof (first s)))) wall)
		(t (nth c (nth r s)))
	)
)

; function returns a list similar to row but where the cth element of row is replaced by v
(defun set-square-row (row c v)
	(append 
		(butlast row (- (sizeof row) c))
		(list v)
		(nthcdr (1+ c) row)
	)
)

; function returns a 2-d list similar to s but where the element in the rth row and cth column is replaced by v
(defun set-square (s r c v)
	(append
		(butlast s (- (sizeof s) r))
		(list (set-square-row (nth r s) c v))
		(nthcdr (1+ r) s)
	)
)

; returns the value of the square the keeper's moving into that originally had value val
(defun move-new (val)
	(cond ((isStar val) keeperstar) ((isBlank val) keeper))
)

; returns the value of the square the keeper's moving from that originally had value val
(defun move-old (val)
	(cond ((isKeeper val) blank) ((isKeeperStar val) star))
)

; returns the value of the square the box's moving to that originally had value val
(defun box-move-new (val)
	(cond ((isStar val) boxstar) ((isBlank val) box))
)

; returns the value of the square the box's moving from that originally had value val
(defun box-move-old (val)
	(cond ((isBox val) blank) ((isBoxStar val) star))
)

; helper function, takes a state s and two element integer list pos of form (r c) and returns the square there
(defun get-short (s pos)
	(get-square s (first pos) (second pos))
)

; d can be one of four directional values: up (-1, 0), right (0, 1), down (1, 0), left (0, -1)
; returns the state that would result from moving the keeper in direction d. If resulting state is illegal, return NIL
; If a box is in direction d from keeper, check if the box can also move in direction d, if not NIL, if yes, move the box in direction d
; and recall try-move with the state s modified to reflect the box's movement.
(defun try-move (s d)
	(let* (
		(curr-pos (getKeeperPosition s 0)) ; 
		(new-pos (list (+ (first curr-pos) (first d)) (+ (second curr-pos) (second d))))
		(curr-val (get-short s curr-pos))
		(new-val (get-short s new-pos)))
		(cond
			((isWall (get-short s new-pos)) NIL)
			((or (isBox (get-short s new-pos)) (isBoxStar (get-short s new-pos)))
				(let* ((new-new-pos (list (+ (first new-pos) (first d)) (+ (second new-pos) (second d)))) (new-new-val (get-short s new-new-pos)))
					(cond
						((or (isWall new-new-val) (isBox new-new-val) (isBoxStar new-new-val)) NIL)
						(t 
							(try-move 
								(set-square 
									(set-square s (first new-pos) (second new-pos) (box-move-old new-val))
									(first new-new-pos) (second new-new-pos) (box-move-new new-new-val)
								)
								d
							)
						)
					)
				)
			) ; keeper trying to move a box
			(t 
				(set-square 
					(set-square s (first curr-pos) (second curr-pos) (move-old curr-val)) ; s 
					(first new-pos) (second new-pos) (move-new new-val) ; r c v
				)
			)
		)
	)
)

; returns a list of the possible next states that could result from performing an action on state s
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s '(-1 0)) (try-move s '(1 0)) (try-move s '(0 -1)) (try-move s '(0 1))))
	 )
    (cleanUpList result);end
   );end let
  );

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s) 0)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;

; num-boxes returns the number of boxes in state s
(defun num-boxes (s &optional (count 0))
	(defun num-boxes-row (row count)
		(cond ((null row) count)
			((isBox (first row)) (num-boxes-row (rest row) (1+ count)))
			(t (num-boxes-row (rest row) count))
		)
	)
	(cond ((null s) count)
		(t (num-boxes (rest s) (+ count (num-boxes-row (first s) 0))))
	)
)

; h1 returns as a heuristic the number of boxes not on a star
; this function is admissible, as it will never overestimate the number of moves needed to finish the game
; there will always be as many or more moves necessary to complete the game than there are un starred boxes
(defun h1 (s)
	(num-boxes s)
)

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;

; is-deadlock returns true if a box at boxpos is cornered by walls in s. I.e. a pair of walls orthogonal to each other are bordering the box
(defun is-deadlock (s boxpos)
	(let ((up (isWall (get-square s (1- (first boxpos)) (second boxpos))))
		(down (isWall (get-square s (1+ (first boxpos)) (second boxpos))))
		(right (isWall (get-square s (first boxpos) (1+ (second boxpos)))))
		(left (isWall (get-square s (first boxpos) (1- (second boxpos)))))
		)
		(cond
			((or (and up left) (and up right) (and down left) (and down right)) T)
			(t NIL)
		)
	)
)

; returns an arbitrarily large heuristic value for deadlocks
(defun deadlock-cost (if-deadlock)
	(if if-deadlock 500 0)
)

; returns a list of positions (r c) of all the squares of value type in game state s
(defun get-items (s type &optional (items NIL) (r 0))
	(defun get-items-row (row type items r c)
		(cond ((null row) items)
			((= type (first row)) (get-items-row (rest row) type (cons (list r c) items) r (1+ c)))
			(t (get-items-row (rest row) type items r (1+ c)))
		)
	)
	(cond ((null s) items)
		(t (get-items (rest s) type (append items (get-items-row (first s) type NIL r 0)) (1+ r)))
	)
)

; helper function for hdeadlock
(defun hdead-h (s boxes &optional (cost 0))
	(cond ((null boxes) cost)
		(t (hdead-h s (rest boxes) (+ cost (deadlock-cost (is-deadlock s (first boxes))))))
	)
)

; heuristic function that returns the number of deadlocked boxes times an arbitrarily large number
(defun hdeadlock (s)
	(hdead-h s (get-items s 2))
)

; returns the manhattan distance between a (r c) pos1 and (r c) pos2
(defun dist (pos1 pos2)
	(+ 
		(abs (- (first pos1) (first pos2))) ; x dist
		(abs (- (second pos1) (second pos2))) ; y dist
	)
)

; returns the minimum manhattan distance between a given box (position) and all the star positions in stars
(defun min-dist-box (box stars &optional (minimum 10000))
	(cond ((null stars) minimum)
		(t (let ((box-to-star (dist box (first stars))))
				(cond
					((< box-to-star minimum) (min-dist-box box (rest stars) box-to-star))
					(t (min-dist-box box (rest stars) minimum))
				)
			)
		)
	)
)

; returns the sum of the minimum distances between each box in boxes and stars
(defun total-dist-boxes-to-stars (boxes stars &optional (sum 0))
	(cond ((null boxes) sum)
		(t (total-dist-boxes-to-stars (rest boxes) stars (+ sum (min-dist-box (first boxes) stars))))
	)
)

; heuristic function that returns the sum of
	; the number of deadlocked boxes times an arbitrarily large number
	; the sum of manhattan distances for each box to its nearest star
(defun h604970765 (s)
	(+ 
		(total-dist-boxes-to-stars (get-items s box) (get-items s star))
		(hdeadlock s)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6)
(setq p0 '(
	(0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	(0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	(0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	(0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	(0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	(1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	(1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	(1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	(1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	(0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	(0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)
))

(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

;(15)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(?)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(?)
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(?)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
