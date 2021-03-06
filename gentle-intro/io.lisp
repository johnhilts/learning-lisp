(defun draw-line (n)
  "draw a line n times; use recursion"
  (cond ((zerop n) nil)
	(t (format t "*")(draw-line (- n 1)))))

(defun draw-box (length height)
  "draw a box with the given dimensions and draw-line"
  (cond ((zerop height) nil)
	(t (draw-line length)
	   (format t "~&")
	   (draw-box length (- height 1)))))

(defun bottles-song (n)
  "99 bottles of beer on the wall"
  (cond ((zerop n)
	 (format t "~&No more bottles left!"))
	(t
	 (format t "~&~S bottles of beer on the wall,~%" n)
	 (format t "~S bottles of beer!~%" n)
	 (format t "Take one down,~%")
	 (format t "Pass it around,~%")
	 (format t "~S bottles of beer on the wall.~%~%" (- n 1))
	 (bottles-song (- n 1)))))

(defun print-board (list)
  "print tic tac board given 9 elements"
  (labels
      (
       (print-cell (element)
	 (if (equal 'nil element)
	     (format t "   ")
	     (format t " ~s " element)))
       (print-board-helper (list n)
	 (cond
	   ((null list) nil)
	   ((equal 1 n)
	    (format t "~&")
	    (print-cell (car list))
	    (format t "|")
	    (print-board-helper (cdr list) (+ n 1)))
	   ((equal 2 n)
	    (print-cell (car list))
	    (format t "|")
	    (print-board-helper (cdr list) (+ n 1)))
	   (t
	    (print-cell (car list))
	    (if (> (length (cdr list)) 0)
		(format t "~%-----------")
		nil)
	    (print-board-helper (cdr list) 1)))))
    (print-board-helper list 1)))

(defun get-gross-pay ()
  "get hours and pay rate from user input"
  (format t "Enter hourly salary: ")
  (let ((salary (read)))
    (format t "Enter hours worked: ")
    (let ((hours (read)))
      (format t "Gross pay: ~s" (* salary hours)))))

(defun cookie-monster ()
  "cookie monster function"
  (format t "Give me cookie!!~%")
  (format t "Cookie?~%")
  (let ((input (read)))
    (cond ((equal 'cookie input) ; note that input is a symbol, not a string!
	   (format t "Thank you! ... Munch munch munch ... BURP"))
	  (t (format t "No want ~s~%" input)
	     (cookie-monster)))))

(defun space-over (n)
  "move cursor to the right n times"
  (cond ((< n 0) (format t "Error!"))
	((zerop n) t)
	(t (format t " ")
	   (space-over (- n 1)))))

(defun test-space-over (n)
  (format t "~%>>>")
  (space-over n)
  (format t "<<<"))

(defun plot-one-point (plotting-string y-val)
  "print plotting-string's contents at position y-val (0 based)"
  (space-over y-val)
  (format t "~a~%" plotting-string))

(defun plot-points (plotting-string y-values)
  "plots string at points in y-values"
  (cond ((null y-values) nil)
	(t (plot-one-point plotting-string (car y-values))
	   (plot-points plotting-string (cdr y-values)))))

(defun generate (m n)
  "generate the collection of integers from m to n"
  (labels
      (
       (generate-helper (m n i)
	 (cond ((equal n i) (cons n nil))
	       ((equal m i) (cons m (generate-helper m n (+ 1 i))))
	       (t (cons i (generate-helper m n (+ 1 i)))))))
    (generate-helper m n m)))
	      
(defun make-graph (func start end plotting-string)
  "makes a graph"
  (plot-points plotting-string (mapcar func (generate start end))))

(defun make-graph-input ()
  "get values for make-graph from user input"
  (format t "Input function name: ")
  (let ((function (read)))
    (format t "Input start: ")
    (let ((start (read)))
      (format t "Input end: ")
      (let ((end (read)))
	(format t "Input plotting string: ")
	(let ((plotting-string (read)))
	  (make-graph function start end plotting-string))))))
  

(defun square (n)
  "square a number"
  (* n n))

(defun dot-prin1 (list)
  "print list in cons format"
  (cond ((null list) (format t "nil")) ; maybe we can fold this into the next line?
	((atom list) (format t "~s" list))
	((listp list)
	 (format t "(")
	 (dot-prin1 (car list))
	 (format t " . ")
	 (dot-prin1 (cdr list))
	 (format t ")"))))
