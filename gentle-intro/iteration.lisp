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

(defun it-check-all-odd (list)
  (do ((cnt 0 (+ 1 cnt)))
      ((equal cnt (length list))
       t)
    (format t "checking ~d~%" (nth cnt list))
    (if (not (oddp (nth cnt list)))
	(return nil))))

(defun launch (n)
  (do ((cnt n (- cnt 1)))
      ((zerop cnt) (format t "Blast off!"))
    (format t "~S..." cnt)))

(defun it-launch (n)
  (dotimes (i n)
    (format t "~s..." (- n i)))
  (format t "Blast off!"))

(defun find-largest (list-of-numbers)
  (let ((largest (first list-of-numbers)))
    (dolist (element (rest list-of-numbers)
	     largest)
      (when (> element largest)
	(setf largest element)))))

(defun find-largest* (list-of-numbers)
  (do* ((x list-of-numbers (cdr x))
	(largest (car list-of-numbers))) ; list of variables
	((null x) largest) ; ending condition
	(when (> (car x) largest) ; actual body
	  (setf largest (car x)))))

(defun book-version-find-largest (list-of-numbers)
  (do* ((largest (first list-of-numbers))
	(z (rest list-of-numbers) (rest z))
	(element (first z) (first z)))
       ((null z) largest)
    (when (> element largest)
      (setf largest element))))

(defun power-of-2 (n) ;2 to the Nth power.
  (let ((result 1))
    (dotimes (i n result)
      (incf result result))))

(defun power-of-2-with-do (n)
  (do ((i 1 (+ i 1)) ; counter
       (result 2)) ; local variable
      ((equal i n) result) ; end condition + consequence (which also gets returned)
    (incf result result)))

(defun first-non-integer (x)
  "Return the first non-integer element of X."
  (do* ((z x (rest z))
	(z1 (first z) (first z)))
       ((null z) 'none)
    (unless (integerp z1)
      (return z1))))

(defun first-non-integer-dolist (list)
  (dolist (x list 'none)
    (unless (integerp x)
      (return x))))

(defun read-a-number ()
  (do ((answer nil))
      (nil) ; termination test nil - endless loop
    (format t "~&Please type a number: ")
    (setf answer (read))
    (if (numberp answer)
	(return answer))
    (format t "~&Sorry, ~S is not a number. Try again." answer)))

(dotimes (i 5 i) ; returns 5
  (format t "~&I = ~S" i))

(defun tricky-dotimes-as-do ()
  (do ((i 0 (incf i)))
      ((equal i 5) i)
      (format t "~&I = ~s" i)))

(defun complement-base (base)
  "return complement of given base"
  (cond
    ((equal 'a base)
     't)
    ((equal 'g base)
     'c)
    ((equal 't base)
     'a)
    ((equal 'c base)
     'g)
    (t nil)))

(defun complement-strand (dna-strand)
  "return complementary strand of a sequence of single-stranded DNA"
  (let ((strand nil))
    (dolist (dna-base dna-strand (nreverse strand))
      (push (complement-base dna-base) strand))))

(defun make-double (dna-strand)
  "return a list of dna-complement pairs"
  (let ((double-strand nil))
    (dolist (dna-base dna-strand (nreverse double-strand))
      (push (list dna-base (complement-base dna-base))
	    double-strand))))

(defun count-bases (dna-strand)
  "accept single or double-strands"
  (labels ((update-double-strand (dna-base counts)
	     (push (list (car dna-base) 1) counts)
	     (push (list (cadr dna-base) 1) counts)
	     counts
	     ))
    (let ((counts nil))
      (dolist (dna-base dna-strand counts)
	(cond ((listp dna-base)
	       (if
		(find-if #'(lambda (e)
			     (or
			      (equal (car e) (car dna-base))
			      (equal (car e) (cadr dna-base))
			      )) counts)
		(mapcar #'(lambda (e)
			    (cond ((equal (car e) (car dna-base))
				   (incf (cadr e)))
				  ((equal (car e) (cadr dna-base))
				   (incf (cadr e)))))
			counts)
		(setf counts (update-double-strand dna-base counts))
		))
	      (t
	       (if (find-if #'(lambda (e)
				(equal (car e) dna-base)) counts)
		   (mapcar #'(lambda (e)
			       (if (equal (car e) dna-base)
				   (incf (cadr e))))
			   counts)
		   (push (list dna-base 1) counts))))))))

(defun prefixp (possible-prefix dna-strand)
  (do ((prefix? possible-prefix (cdr prefix?))
       (dna dna-strand (cdr dna))
       (is-prefix t))
      ((null prefix?) is-prefix)
    (unless (equal (car prefix?) (car dna))
      (setf is-prefix nil))))

(defun appearsp (search-dna dna-strand)
  "does dna-strand contain search-dna?"
  (do ((dna dna-strand (cdr dna)))
      ((null dna) nil)
    (if (prefixp search-dna dna)
	(return t))))

(defun coverp (repeated-dna? dna-strand)
  "is dna strand a collection of repeated strands?"
  (if (and
       (prefixp repeated-dna? dna-strand)
       (equal (mod (length dna-strand) (length repeated-dna?)) 0))
      (let ((check-strand nil))
	(dotimes (i (length dna-strand) t)
	  (push (car dna-strand) check-strand)
	  (setf dna-strand (cdr dna-strand))
	  (if (equal (mod (length check-strand) (length repeated-dna?)) 0)
	      (if (equal repeated-dna? (reverse check-strand))
		  (setf check-strand nil)
		  (return nil)))))))

(defun prefix (n dna-strand)
  "get n left most dna bases of dna-strand"
  (do ((dna dna-strand (cdr dna))
       (i 0 (+ 1 i))
       (prefix-strand nil))
      ((or
	(null dna)
	(equal i n))
       (nreverse prefix-strand))
    (push (car dna) prefix-strand)))

(defun kernel (dna-strand)
  "get the shortest strand that will cover the entire sequence"
  (do ((dna dna-strand (cdr dna))
       (prefix nil))
      ((null dna) (reverse prefix))
    (push (car dna) prefix)
    (when (coverp (reverse prefix) dna-strand)
      (return (reverse prefix)))))

(defun book-kernel (strand) ; very elegant!
  (do ((i 1 (+ i 1)))
      ((coverp (prefix i strand) strand)
       (prefix i strand))))
