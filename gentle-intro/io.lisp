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
