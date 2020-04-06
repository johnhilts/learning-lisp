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
  (labels (
	   (show-formatted-line (line)
	     (format t "~s~%" line)
	     (let ((len (length line)))
	       (dotimes (i len)
		 (let ((clear
			(gethash
			 (gethash
			  (aref line i) *encipher-table*) *decipher-table*)))
		   (if clear
		       (format t "~s" clear))))))

	   (show-each-line (lines)
	     (cond ((null lines) (format t "~%"))
		   (t (show-formatted-line (car lines))
		      (show-each-line (cdr lines))))))

    (show-each-line crypto-text)))

(defun get-first-char (x)
  (char-downcase
   (char (format nil "~A" x) 0))) ; AFAICT format isn't necessary for this
