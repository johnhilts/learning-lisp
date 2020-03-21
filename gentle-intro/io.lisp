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
