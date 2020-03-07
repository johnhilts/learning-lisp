(defun my-add-up (x)
  "add up numbers recursively"
  (cond ((null x) 0)
	(t (+ (car x) (my-add-up (cdr x))))))

(defun my-alloddp (x)
  "check all elements are odd recursively"
  (cond ((null x) t)
	((and
	  (oddp (car x))
	  (my-alloddp (cdr x))))
	(t nil)))
