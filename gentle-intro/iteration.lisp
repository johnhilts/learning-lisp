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
