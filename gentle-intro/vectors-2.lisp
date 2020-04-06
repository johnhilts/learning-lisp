(defvar crypto-text nil)
(defvar *encipher-table* nil)
(defvar *decipher-table* nil)


(defun init-crypto-text ()
  (setf crypto-text '("zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf" "enlpo pib slafml pvv bfwkj")))

(defun init-crypto-hash-tables ()
  (setf *encipher-table* (make-hash-table))
  (setf *decipher-table* (make-hash-table)))

(defun make-substitution (code clear)
  (setf (gethash code *decipher-table*) clear)
  (setf (gethash clear *encipher-table*) code))

(defun undo-substitution (code)
  (let ((clear (gethash code *decipher-table*)))
    (setf (gethash code *decipher-table*) nil)
    (setf (gethash clear *encipher-table*) nil)))

(defun clear ()
  (clrhash *encipher-table*)
  (clrhash *decipher-table*))

(defun decipher-string (encoded-string)
  (let* ((len (length crypto-text))
	 (new-string (make-string len :initial-element #\Space)))
    (dotimes (i len)
      (let* ((code (gethash encoded-string *encipher-table*))
	     (clear (gethash code *decipher-table*)))
	(if clear
	    (setf (aref new-string i) clear))))))

(defun show-line ()
  (labels ((show-each-line (lines)
	     (cond ((null lines) (format t "~%"))
		   (t
		    (let ((text (car lines)))
		      (format t "~s~%" text)
		      (let ((len (length text)))
			(dotimes (i len)
			  (let ((clear
				 (gethash
				  (gethash
				   (aref text i) *encipher-table*) *decipher-table*)))
			    (if clear
				(Format t "~s" clear))
			    ))))
		    (show-each-line (cdr lines))))))
    (show-each-line crypto-text)))
