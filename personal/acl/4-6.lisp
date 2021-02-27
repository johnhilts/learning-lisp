(defun map-hash-table-to-alist (ht)
  "convert a hashtable into a new alist"
  (let ((alist ()))
    (maphash
     #'(lambda (k v)
	 (setf alist (acons k v alist)))
     ht)
    (reverse alist)))

(defun test-ht-2-al ()
  (let ((ht (make-hash-table)))
    (setf (gethash 'one ht) 1
	  (gethash 'two ht) 2
	  (gethash 'three ht) 3)
    (maphash #'(lambda (k v) (format t "key: ~a value: ~a~%" k v)) ht)
    (mapc
     #'(lambda (e)
	 (format t "key: ~a value: ~a~%" (car e) (cdr e)))
     (map-hash-table-to-alist ht))))

(test-ht-2-al)

(defun map-alist-to-hash-table (alist)
  "convert an alist into a hashtable"
  (let ((ht (make-hash-table)))
    (mapc
     #'(lambda (e)
	 (setf (gethash (car e) ht) (cdr e)))
     alist)
    ht))

(defun test-al-2-ht ()
  (let ((alist '((one . 1) (two . 2) (three . 3))))
    (mapc
     #'(lambda (e)
	 (format t "key: ~a value: ~a~%" (car e) (cdr e)))
     alist)
    (maphash #'(lambda (k v) (format t "~&key: ~a value: ~a~%" k v)) (map-alist-to-hash-table alist))))

(test-al-2-ht)
