(defun expression-matter (a b c)
  "determine which combination yields the max result"
  (labels ((rec-max (list)
	     (cond ((null list) 0)
		   (t (max (car list) (rec-max (cdr list)))))))
    (let ((funs (list
		 #'(lambda () (* a b c))
		 #'(lambda () (+ a b c))
		 #'(lambda () (* (+ a b) c))
		 #'(lambda () (* a (+ b c))))))
      (rec-max (mapcar #'(lambda (e) (funcall e)) funs)))))


