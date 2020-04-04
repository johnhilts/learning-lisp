(defvar *total-points* 0)

(defvar *hist-array* (make-array 11))

(defun new-histogram (number-of-bins)
  (setf *total-points* 0)
  (setf *hist-array* (make-array number-of-bins)))

(defun record-value (n)
  (if (and (>= n 0)
	   (< n (length *hist-array*)))
      (progn
	(incf (aref *hist-array* n))
	(incf *total-points*))
      (format t "error - input out of range")))
