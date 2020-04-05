(defvar *total-points* 0)

(defvar *hist-array* (make-array 11))

(defun new-histogram (number-of-bins)
  (setf *total-points* 0)
  (setf *hist-array* (make-array number-of-bins))
  (dotimes (i 200)
    (record-value (random number-of-bins)))))

(defun record-value (n)
  (if (and (>= n 0)
	   (< n (length *hist-array*)))
      (progn
	(incf (aref *hist-array* n))
	(incf *total-points*))
      (format t "error - input out of range")))

(defun print-hist-line (n)
  (let ((count (aref *hist-array* n)))
    (format t "~&~2s [ ~3s] " n count)
    (dotimes (i count)
      (format t "*"))))

(defun print-histogram ()
  (dotimes (i (length *hist-array*))
    (print-hist-line i)))
  
