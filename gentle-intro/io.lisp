(defun draw-line (n)
  "draw a line n times; use recursion"
  (cond ((zerop n) nil)
	(t (format t "*")(draw-line (- n 1)))))
