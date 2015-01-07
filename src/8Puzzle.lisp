;;;;;;;;;;;;;;;;;;;;;;;;;;; Functions to simplify testing ;;;;;;;;;;;;;;;;;;;;;;

(defun solve-8-puzzle (state heuristic)
  (general-search state #'next-positions heuristic #'goal-test :samep #'equal :enqueue #'enqueue-priority :key #'priority-queue-key))

(defun solve-8-puzzle-with-all-heuristics (state)
  (let ((misplaced (multiple-value-list (general-search state #'next-positions #'misplaced-tiles #'goal-test 
							    :samep #'equal :enqueue #'enqueue-priority :key #'priority-queue-key)))
	(manhattan (multiple-value-list (general-search state #'next-positions #'manhattan-distance #'goal-test 
							    :samep #'equal :enqueue #'enqueue-priority :key #'priority-queue-key)))
	(manhattanpair (multiple-value-list (general-search state #'next-positions #'manhattan-pair-distance #'goal-test 
							    :samep #'equal :enqueue #'enqueue-priority :key #'priority-queue-key))))
	(print 'Misplaced-Tiles)
	(print (car misplaced))
	(print (cadr misplaced))
	(print 'Manhattan-Distance)
	(print (car manhattan))
	(print (cadr manhattan))
	(print 'Manhattan-Pair-Distance)
	(print (car manhattanpair))
	(print (cadr manhattanpair))
	'Finished
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;; Random Initial States ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Since it is possible to generate unsolvable states, the easiest way to generate
; some random state would be to take the goal state and apply my movement functions
; to it some random amount of times.
; Is modified to also use the extra credit heuristic.
(defun solve-n-random-8-puzzles (n)
  (if (= n 0)
      'Done
      (let* ((start '(0 1 2 3 4 5 6 7 8))
	     (randomized (randomize start)))
	(print randomized)
	(solve-8-puzzle-with-all-heuristics randomized)
	(print 'Next-Iteration)
	(solve-n-random-8-puzzles (1- n)))
  )
)

; Does a minimum of 20 moves + average of 100 moves from the goal state
(defun randomize (start)
  (let ((moves (list #'up #'down #'left #'right))
	(temp start))
    (do ((number-of-moves (+ 20 (random 201)) (1- number-of-moves)))
	((= number-of-moves 0) temp)
      (let ((opnum (random 4))
	    (blank (current-abstract-position temp)))
	(setf temp (funcall (elt moves opnum) temp blank))
      )
    ))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; A* search ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Key for Priority Queue
(defun priority-queue-key(node)
  (+ (node-path-cost node) (node-heuristic node))
)

; Misplaced tiles Heuristic
; According to lecture notes, does not count blank position
(defun misplaced-tiles (state)
  (let ((hofn 0)
	(index 0))
    (dolist (tile state hofn)
      (unless (or (eql tile index) (= tile 0))
	  (incf hofn))
      (incf index)
    ))
)

; Manhattan distance Heuristic
; My representation has the nice property (from the fact that index in my representation 
; where the tile should be IS the tile number itself. Ex. tile 7 at index 7) that:
; | index/3 - tilename/3 | = number of vertical moves to move into right position
; | index%3 - tilename%3 | = number of horizontal moves to move into right position
; That is, the sum of these IS the manhattan distance.
; The floor function returns both division and modulus in one fell swoop
; My "origin" here is 0,0 or the top,leftmost tile position
(defun manhattan-distance (state) 
    (do ((hofn 0)
	(index 0 (1+ index)))
	((= index 9) hofn)
      (let* ((tile (elt state index))
	     (index-dist-from-origin (multiple-value-list (floor index 3)))
	     (tile-dist-from-origin (multiple-value-list (floor tile 3)))
	     (vdist (abs (- (car index-dist-from-origin) (car tile-dist-from-origin))))
	     (hdist (abs (- (cadr index-dist-from-origin) (cadr tile-dist-from-origin)))))
	(unless (or (eql tile index) (= tile 0))
	  (setf hofn (+ hofn hdist vdist)))
      ))
)


; Manhattan Pair Distance Heuristic
; Source:
; Bauer, Bernard. (1995, January 14). The manhattan pair distance for the 15-puzzle. Retrieved from http://www.google.com/url?sa=t&rct=j&q=manhattan pair distance&source=web&cd=1&ved=0CBsQFjAA&url=http://disi.unitn.it/~montreso/asd/progetti/2007-08/progetto2/The_Manhattan_Pair_Distance_Heuristic_for_the_15-puzzle.pdf&ei=xvqsTtf4HamBsgK_1oWUDw&usg=AFQjCNEQJ5rd5Bjb-XJXQI1RzgFLy235BA&cad=rja 
; Check the README for the algorithm details.
; Because of setf, you cannot use lexical analysis and so I must check the same conditions even inside statements that I entered on that condition.
; The basic gist of the code is as follows. I maintain a seen list of rows and columns that I have paired up (so that I don't pair up more than once
; in any row or column). I then step through all tiles and see if I can pair it with anything else on its row and column. If I can, I add 2 to the pair
; distance and put both the pair members into the respective seen list. In an 8-puzzle, this basically means there is only at most one pair per row and 
; column.
(defun manhattan-pair-distance (state)
  (let ((manhattan (manhattan-distance state))
	(row-list nil)
	(col-list nil))
    (do ((pd 0)
	 (index 0 (1+ index)))
	 ((= index 9) (+ pd manhattan))
	(let ((tile (elt state index)))
	  (unless (= tile 0)
	    (let ((other-row-members (find-row-members index state)))
	      (unless (or (find (car other-row-members) row-list) (find tile row-list))
		(when (is-before-on-goal-row tile index (car other-row-members) state)
		  (setf pd (+ pd 2))
		  (setf row-list (cons tile (cons (car other-row-members) row-list))))
	      )
	      (unless (or (find (cadr other-row-members) row-list) (find tile row-list))
		(when (is-before-on-goal-row tile index (cadr other-row-members) state)
		  (setf pd (+ pd 2))
		  (setf row-list (cons tile (cons (cadr other-row-members) row-list))))
	      ))
	    (let ((other-col-members (find-col-members index state)))
	      (unless (or (find (car other-col-members) col-list) (find tile col-list))
		(when (is-before-on-goal-col tile index (car other-col-members) state)
		  (setf pd (+ pd 2))
		  (setf col-list (cons tile (cons (car other-col-members) col-list))))
	      )
	      (unless (or (find (cadr other-col-members) col-list) (find tile col-list))
		(when (is-before-on-goal-col tile index (cadr other-col-members) state)
		  (setf pd (+ pd 2))
		  (setf col-list (cons tile (cons (cadr other-col-members) col-list))))
	      ))
	 ))
    ))
)
; Checks if current tile and member is a pair we are looking for
(defun is-before-on-goal-row (tile index member state)
  (let ((member-index (position member state)))
    (when (and (< member-index index) (> member tile) (in-goal-row tile member index member-index))
      'T)
  )
)

(defun is-before-on-goal-col (tile index member state)
  (let ((member-index (position member state)))
    (when (and (< member-index index) (> member tile) (in-goal-col tile member index member-index))
      'T)
  )
)

(defun in-goal-row (tile member index member-index)
  (let ((goal-row-tile (* (floor tile 3) 3))
	(goal-row-member (* (floor member 3) 3))
	(goal-row-index (* (floor index 3) 3))
	(goal-row-member-index (* (floor member-index 3) 3)))
    (when (= goal-row-tile goal-row-member goal-row-index goal-row-member-index)
      'T)
  )
)

; Find the goal column starting position for both tile and index and see if their indexes are in that column
; I choose to use the filter function because I used it in an example above. Modulus would be a better choice here.
(defun in-goal-col (tile member index member-index)
  (let ((goal-col-tile (cadr (multiple-value-list (floor tile 3))))
	(goal-col-member (cadr (multiple-value-list (floor member 3))))
	(goal-col-index (cadr (multiple-value-list (floor index 3))))
	(goal-col-member-index (cadr (multiple-value-list (floor member-index 3)))))
    (when (= goal-col-tile goal-col-member goal-col-index goal-col-member-index)
      'T)
  )
)

(defun find-row-members (index state)
  (let* ((row-start (* (floor index 3) 3))
	 (row-members-index (row-index-list index row-start)))
    (mapcar (lambda (index) (elt state index)) row-members-index)
  )
)

(defun find-col-members (index state)
  (let* ((col-start (cadr (multiple-value-list (floor index 3))))
	 (col-members-index (col-index-list index col-start)))
    (mapcar (lambda (index) (elt state index)) col-members-index)
  )
)

(defun row-index-list (index row-start)
  (let ((temp nil)
	(end (+ row-start 3)))
    (do ((i row-start (1+ i)))
	 ((= i end) temp)
      (when (/= i index)
	(setf temp (cons i temp)))
    ))
)

(defun col-index-list (index col-start)
  (let ((temp nil))
    (do ((i col-start (+ i 3)))
	 ((> i 8) temp)
      (when (/= i index)
	(setf temp (cons i temp)))
    ))
)
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;; Problem Representation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Definition of 8 puzzle problem - List of all possible locations of blank (0)
; That is the problem representation is: 
; Given (a b c d e 0 f g h), where 0 is the blank and a b c d e f g h are any of the other tiles, 
; this corresponds to 5, for the location of the blank tile. It is not relevant what the 
; other tiles are, nor is representing the problem in the same way as a state.

; As for states, the state representation is a flat list with the locations of each piece on the board
; at that index in the list. The indices of the list are mapped to the board as follows:
; 0  1  2
; 3  4  5
; 6  7  8
; Next states are computed by the next-positions functions. Costs are not in the 
; problem definition as they are all unit. Heuristic values will be 
; determined when a node is created.
; I have also chosen to use 0 to represent the blank (not to be confused with
; position 0 on the board.

; A list of all possible locations of the blank piece
; I could have used a more complex representation such as
; '(0 a b c d e f g h)
; '(a 0 b c d e f g h)
; and so on but since we don't care about what else in the other slots, it serves
; to store only the blank location.

(defun 8-puzzle ()
  '(0 1 2 3 4 5 6 7 8)
)

; Using definition of 8-puzzle just to link it to the problem
; The elt is not needed. It is there to use the definition of 
; the problem. Note that this happens only because the index
; is equal to the tile it represents and blank is 0 (by design)
(defun current-abstract-position (state)
  (elt (8-puzzle) (position 0 state))
)

; Goal predicate
(defun goal-test (state)
  (equal state '(0 1 2 3 4 5 6 7 8))
)

; Same predicate
(defun same-test (state1 state2)
  (equal state1 state2)
)

; Removing no action moves here instead of in the closed list for efficiency.
; Returns a list containing upto 4 lists, where the car of each list
; contains an action (movement of blank tile) and the cadr is the resulting state
(defun next-positions (state)
  (let* ((blank (current-abstract-position state))
	(u (list 'up (up state blank)))
	(d (list 'down (down state blank)))
	(l (list 'left (left state blank)))
	(r (list 'right (right state blank))))
    (filter (list u d l r) state))
)

;;;;;;;;;;;;;;;;;;;;;;;;;; Helpers for next-positions function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun filter (moves state)
  (cond
    ((null moves) nil)
    ((equal (cadar moves) state) (filter (cdr moves) state))
    (t (cons (car moves) (filter (cdr moves) state)))
  )
)

(defun up (state pos)
  (let ((temp (copy-list state)))
    (progn
      (when (> pos 2)
	  (rotatef (elt temp pos) (elt temp (- pos 3))))
      temp))
)

(defun down (state pos)
  (let ((temp (copy-list state)))
    (progn
      (when (< pos 6)
	  (rotatef (elt temp pos) (elt temp (+ pos 3))))
      temp))
)

(defun left (state pos)
  (let ((temp (copy-list state)))
    (progn
      (when (not (or (= pos 0) (= pos 3) (= pos 6)))
	  (rotatef (elt temp pos) (elt temp (- pos 1))))
      temp))
)

(defun right (state pos)
  (let ((temp (copy-list state)))
    (progn
      (when (not (or (= pos 2) (= pos 5) (= pos 8)))
	  (rotatef (elt temp pos) (elt temp (+ pos 1))))
      temp))
)

;;;;;;;;;;;;; Graph Search ;;;;;;;;;;;;;;;

; Successor function returns action-state as cost is 1
(defun expand (successor heuristic node)
  (let ((doubles (funcall successor (node-state node))))
    (mapcar (lambda (action-state)
	      (let ((action (car action-state))
		    (state (cadr action-state)))
		(make-node :state state 
			   :parent node
			   :action action 
			   :path-cost (+ (node-path-cost node) 1)
			   :heuristic (funcall heuristic state)
			   :depth (1+ (node-depth node)))
		))
	    doubles)
    ))

(defun action-sequence (node &optional (actions nil))
  (if (node-parent node)
    (action-sequence (node-parent node) (cons (node-action node) actions))
    actions
    ))

(defstruct node 
  (state nil)
  (parent nil)
  (action nil)
  (path-cost 0)
  (heuristic 0)
  (depth 0))

(defvar *nodes-expanded*)

(defun general-search (initial-state successor heuristic goalp
		       &key (samep #'eql)
		            (enqueue #'enqueue-LIFO) 
		            (key #'identity))
  (setf *nodes-expanded* 0)
  (let ((fringe (make-q :enqueue enqueue :key key)))
    (q-insert fringe (list (make-node :state initial-state)))
    (values 
     (graph-search fringe nil successor heuristic goalp samep)
     *nodes-expanded*)
    ))

(defun graph-search (fringe closed successor heuristic goalp samep)
  (unless (q-emptyp fringe)
    (let ((node (q-remove fringe)))
      (cond ((funcall goalp (node-state node)) 
	     (action-sequence node))
            ((member (node-state node) closed 
		     :test samep :key #'node-state)
	     (graph-search fringe closed successor heuristic goalp samep))
            (t 
	     (let ((successors (expand successor heuristic node)))
	       (setf *nodes-expanded* 
		     (+ *nodes-expanded* (length successors)))
	       (graph-search (q-insert fringe successors)
			     (cons node closed)
			     successor heuristic goalp samep)))
            ))
    ))

;;;; The Queue datatype

;;; We can remove elements form the front of a queue.  We can add elements in
;;; three ways: to the front (LIFO, or stack), to the back (FIFO, or simple queue), or by priority (priority queue).
;;; This is done with the following enqueing functions specified when we make the queue, which make use of the
;;; following implementations of the elements:
;;;   ENQUEUE-LIFO - elements are a list
;;;   ENQUEUE-FIFO   - elements are a list
;;;   ENQUEUE-PRIORITY - elements are a heap, implemented as an array
;;; The best element in the queue is always in position 0.
;;; For priority queues, we can specify a key function that should return the priority value of an element.
;;; For FIFO queues, we maintain a pointer to the last element for efficient enqueuing.

(defstruct q
  (enqueue #'enqueue-FIFO)
  (key #'identity)
  (last nil)
  (elements nil))

;;;; Operations on Queues

(defun q-emptyp (q)
  "Returns T if queue is empty."
  (= (length (q-elements q)) 0))       ; (length x) works for both lists and arrays with fill-pointers

(defun q-front (q)
  "Returns the element at the front of the queue."
  (elt (q-elements q) 0))              ; (elt x n) works for both lists and arrays

(defun q-remove (q)
  "Removes the element from the front of the queue and returns it."
  (if (listp (q-elements q))
      (pop (q-elements q))             ; (pop x) alters x by removing the car, then returns the item removed
    (heap-pop (q-elements q) (q-key q))))

(defun q-insert (q items)
  "Inserts the items into the queue, according to the queue's enqueuing function."
  (funcall (q-enqueue q) q items)
  q)

;;;; The Three Enqueing Functions

(defun enqueue-LIFO (q items)
  "Adds a list of items to the front of the queue."
  (setf (q-elements q) (nconc items (q-elements q)))  ; (nconc x y) is destructive version of (append x y)
  items
  )

(defun enqueue-FIFO (q items)
  "Adds a list of items to the end of the queue."
  (if (q-emptyp q) 
      (setf (q-elements q) items)
    (setf (cdr (q-last q)) items))
  (setf (q-last q) (last items))
  items
  )

(defun enqueue-priority (q items)
  "Inserts the items by priority determined by the queue's key function."
  ;; If first insert, create the heap
  (when (null (q-elements q))
    (setf (q-elements q) (make-heap)))
  ;; Now insert the items
  (mapc (lambda (item)
	  (heap-insert (q-elements q) item (q-key q)))
	items)
  )

;;;; The Heap Implementation of Priority Queues

;;; The idea is to store a heap in an array so that the heap property is
;;; maintained for all elements: heap[Parent(i)] <= heap[i].  Note that we
;;; start at index 0, not 1, and that we put the lowest value at the top of
;;; the heap, not the highest value.

(defun heap-val (heap i key) (funcall key (elt heap i)))
(defun heap-parent (i) (floor (1- i) 2))
(defun heap-left (i) (+ 1 i i))
(defun heap-right (i) (+ 2 i i))
(defun heap-leafp (heap i) (> i (1- (floor (length heap) 2))))

(defun heapify (heap i key)
  "Assume that the children of i are heaps, but that heap[i] may be 
  larger than its children.  If it is, moves heap[i] down where it belongs."
  (unless (heap-leafp heap i)
    (let ((l (heap-left i))
	  (r (heap-right i)))
      (let ((smaller-child (if (and (< r (length heap))
				    (< (heap-val heap r key) (heap-val heap l key)))
			       r l)))
	(when (> (heap-val heap i key) (heap-val heap smaller-child key))
	  (rotatef (elt heap i) (elt heap smaller-child))    ; (rotatef x y) swaps values of x and y
	  (heapify heap smaller-child key))))
    ))

(defun heap-pop (heap key)
  "Pops the best (lowest valued) item off the heap."
  (let ((min (elt heap 0)))
    (setf (elt heap 0) (elt heap (1- (length heap))))
    (decf (fill-pointer heap))        ; (decf x) decrements x
    (heapify heap 0 key)
    min))

(defun heap-insert (heap item key)
  "Puts an item into a heap."
  (vector-push-extend nil heap)       ; (vector-push-extend value array) adds the value to the next
                                      ; available position in the array, incrementing the fill-pointer
                                      ; and increasing the size of the array if necessary.
  (setf (elt heap (heap-find-pos heap (1- (length heap)) (funcall key item) key)) 
	item)

  )

(defun heap-find-pos (heap i val key)
  "Bubbles up from i to find position for val, moving items down in the process."
  (cond ((or (zerop i) (< (heap-val heap (heap-parent i) key) val))
	 i)
	(t
	 (setf (elt heap i) (elt heap (heap-parent i)))
	 (heap-find-pos heap (heap-parent i) val key))
	))

(defun make-heap (&optional (size 100))
  (make-array size :fill-pointer 0 :adjustable t))
