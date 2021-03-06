(setf *friend-total* 0) ; compiler warning :(

(setf *friends* nil)

(defun meet (person)
  (cond ((equal person (first *friends*)) 'we-just-met)
	((member person *friends*) 'we-know-each-other)
	(t (push person *friends*)
	   (incf *friend-total*)
	   'pleased-to-meet-you)))

(defun forget (friend)
  "forget a friend"
  (cond
    ((member friend *friends*)
     (setf *friends* (remove friend *friends*))
     (decf *friend-total*))
    (t (format t "~s is not your friend!" friend))))

(defun make-board ()
  (list 'board 0 0 0 0 0 0 0 0 0))

(defun convert-to-letter (v)
  (cond ((equal v 1) "O")
	((equal v 10) "X")
	(t " ")))

(defun print-row (x y z)
  (format t "~& ~A | ~A | ~A"
	  (convert-to-letter x)
	  (convert-to-letter y)
	  (convert-to-letter z)))

(defun print-board (board)
  (format t "~%")
  (print-row
   (nth 1 board) (nth 2 board) (nth 3 board))
  (format t "~& -----------")
  (print-row
   (nth 4 board) (nth 5 board) (nth 6 board))
  (format t "~& -----------")
  (print-row
   (nth 7 board) (nth 8 board) (nth 9 board))
  (format t "~%~%"))

(defun make-move (player pos board)
  (setf (nth pos board) player)
  board)

(setf *triplets*
      '((1 2 3) (4 5 6) (7 8 9) ;Horizontal triplets.
	(1 4 7) (2 5 8) (3 6 9) ;Vertical triplets.
	(1 5 9) (3 5 7))) ;Diagonal triplets.

(setf *opponent* 1)
(setf *computer* 10)

(defun sum-triplet (board triplet)
  (+ (nth (first triplet) board)
     (nth (second triplet) board)
     (nth (third triplet) board)))

(defun compute-sums (board)
  (mapcar #'(lambda (triplet)
	      (sum-triplet board triplet))
	  *triplets*))

(defun winner-p (board)
  (let ((sums (compute-sums board)))
    (or (member (* 3 *computer*) sums)
	(member (* 3 *opponent*) sums))))

(defun read-a-legal-move (board)
  (format t "~&Your move: ")
  (let ((pos (read)))
    (cond ((not (and (integerp pos)
		     (<= 1 pos 9)))
	   (format t "~&Invalid input.")
	   (read-a-legal-move board))
	  ((not (zerop (nth pos board)))
	   (format t
		   "~&That space is already occupied.")
	   (read-a-legal-move board))
	  (t pos))))

(defun board-full-p (board)
  (not (member 0 board)))

(defun pick-random-empty-position (board)
  (let ((pos (+ 1 (random 9))))
    (if (zerop (nth pos board))
	pos
	(pick-random-empty-position board))))

(defun random-move-strategy (board)
  (list (pick-random-empty-position board)
	"random move"))

(defun choose-best-move (board) ;Second version.
  (or (make-three-in-a-row board)
      (block-opponent-win board)
      (random-move-strategy board)))

(defun computer-move (board)
  (let* ((best-move (choose-best-move board))
	 (pos (first best-move))
	 (strategy (second best-move))
	 (new-board (make-move
		     *computer* pos board)))
    (format t "~&My move: ~S" pos)
    (format t "~&My strategy: ~A~%" strategy)
    (print-board new-board)
    (cond ((winner-p new-board)
	   (format t "~&I win!"))
	  ((board-full-p new-board)
	   (format t "~&Tie game."))
	  (t (opponent-move new-board)))))

(defun opponent-move (board)
  (let* ((pos (read-a-legal-move board))
	 (new-board (make-move
		     *opponent*
		     pos
		     board)))
    (print-board new-board)
    (cond ((winner-p new-board)
	   (format t "~&You win!"))
	  ((board-full-p new-board)
	   (format t "~&Tie game."))
	  (t (computer-move new-board)))))

(defun play-one-game ()
  (if (y-or-n-p "Would you like to go first? ")
      (opponent-move (make-board))
      (computer-move (make-board))))


(defun find-empty-position (board squares)
  (find-if #'(lambda (pos)
	       (zerop (nth pos board)))
	   squares))

(defun win-or-block (board target-sum)
  (let ((triplet (find-if
		  #'(lambda (trip)
		      (equal (sum-triplet board trip)
			     target-sum))
		  *triplets*)))
    (when triplet
      (find-empty-position board triplet))))

(defun make-three-in-a-row (board)
  (let ((pos (win-or-block board
			   (* 2 *computer*))))
    (and pos (list pos "make three in a row"))))

(defun block-opponent-win (board)
  (let ((pos (win-or-block board
			   (* 2 *opponent*))))
    (and pos (list pos "block opponent"))))

(defun ugly (x y)
  (when (> x y)
    (setf temp y)
    (setf y x)
    (setf x temp))
  (setf avg (/ (+ x y) 2.0))
  (setf pct (* 100 (/ avg y)))
  (list 'average avg 'is
	pct 'percent 'of 'max y))

(defun not-ugly (x y)
  (let* ((max-x (min x y))
	 (max-y (max x y))
	 (avg (/ (+ max-x max-y) 2.0))
	 (pct (* 100 (/ avg max-y))))
    (list 'average avg 'is
	  pct 'percent 'of 'max max-y)))

(defun analyze-profit (price commission-rate)
  (let* ((commission (* price commission-rate))
	 (result
	  (cond ((> commission 100) 'rich)
		((< commission 100) 'poor))))
    (break "Value of RESULT is ~S" result)
    (format t "~&I predict you will be: ~S"
	    result)
    result))

(defun nchop (list) ; won't work with constants; need to pass an assigned variable
  "reduce any non nil list to 1 element"
  (setf (cdr list) nil))

(defun ntack (list item-to-append)
  "destructively append and item to a list"
  (nconc list (list item-to-append)))
