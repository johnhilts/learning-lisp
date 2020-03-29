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
  
(defun it-length (list)
  "iterative version of length"
  (let ((length 0))
    (dolist (x list length)
      (incf length))))

(defun it-nth (i list)
  (let ((counter 0))
    (dolist (x list)
      (if (equal i counter)
	  (return x)
	  (incf counter)))))

(defun it-union (list1 list2)
  (let ((result list2))
    (dolist (x list1 result)
      (push x result))))

(defun it-reverse (list)
  (let ((result nil))
    (dolist (x list result)
      (push x result))))
