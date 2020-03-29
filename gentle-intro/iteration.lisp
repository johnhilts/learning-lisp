(dolist (x ’(red blue green) ’flowers)
  (format t "~&Roses are ~S." x))

(defun it-member (item list)
  (dolist (x list)
    (if (equal x item)
	(return t))))

(defun it-assoc (key dictionary)
  (dolist (entry dictionary)
    (if (equal key (car entry))
	(return entry))))

(defun check-all-odd (list-of-numbers)
  (dolist (e list-of-numbers t)
    (format t "~&Checking ~S..." e)
    (if (not (oddp e)) (return nil))))

(defun check-all-odd-recursive (list-of-numbers)
  (cond
    ((null list-of-numbers)
     t)
    (t (format t "~&Checking ~d..." (car list-of-numbers))
       (and
	(oddp (car list-of-numbers))
	(check-all-odd-recursive (cdr list-of-numbers))))))
  
