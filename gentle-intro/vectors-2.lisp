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

(defun undo-substitution (clear)
  (let ((code (gethash clear *decipher-table*)))
    (setf (gethash code *encipher-table*) nil)
    (setf (gethash clear *decipher-table*) nil)))

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

(defun show-line (cryptogram)
  (labels (
	   (show-formatted-line (line)
	     (format t "~a~%" line)
	     (let ((len (length line)))
	       (dotimes (i len)
		 (let ((clear
			 (gethash (aref line i) *decipher-table*)))
		   (if clear
		       (format t "~a" clear)
		       (format t " ")))))
	     (format t "~%"))

	   (show-each-line (lines)
	     (cond ((null lines) (format t "~%"))
		   (t (show-formatted-line (car lines))
		      (show-each-line (cdr lines))))))

    (show-each-line cryptogram)))

(defun get-first-char (x)
  (char-downcase
   (char (format nil "~A" x) 0))) ; AFAICT format isn't necessary for this

(defun read-letter ()
  (format t "~&Input a letter:~%")
  (let ((input (read)))
    (cond ((or
	    (equal input 'end)
	    (equal input 'undo))
	   input)
	  (t (get-first-char input)))))

(defun sub-letter (code-string)
  "substitute a letter"
  (let* ((code (get-first-char code-string))
	 (clear (gethash code *decipher-table*)))
    (if clear
	(format t "'~a' has already been deciphered as '~a'." code clear)
	(progn
	  (format t "What does ~a decipher to?~%" code)
	  (let* ((decipher-to-string (read))
		 (decipher-to (get-first-char decipher-to-string))
		 (clear (gethash decipher-to *encipher-table*)))
	    (if clear
		(format t "'~a' already deciphers to ~a~%" clear decipher-to)
		(make-substitution code decipher-to)))))))

(defun print-hash-table (hash-table)
  (maphash #'(lambda (key value)
	       (format t "~4S => ~S~%" key value))
	   hash-table))

(defun undo-letter ()
  (format t "Undo which letter?~%")
  (let* ((undo (get-first-char (read)))
	 (clear (gethash undo *decipher-table*)))
    (if clear
	(undo-substitution undo)
	(format t "No entry found for '~a'." undo))))

(defun solve (cryptogram)
  (show-line cryptogram)
  (format t "Sustitute which letter?~%")
  (let ((input (read-letter)))
    (cond ((equal 'end input) t)
	  ((equal 'undo input)
	   (undo-letter)
	   (solve cryptogram))
	  ((characterp input)
	   (sub-letter input)
	   (solve cryptogram))
	  (t (format t "Error")))))
