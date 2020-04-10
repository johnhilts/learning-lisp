(defmacro ppmx (form)
  "Pretty prints the macro expansion of FORM."
  `(let* ((exp1 (macroexpand-1 ',form))
	  (exp (macroexpand exp1))
	  (*print-circle* nil))
     (cond ((equal exp exp1)
	    (format t "~&Macro expansion:")
	    (pprint exp))
	   (t (format t "~&First step of expansion:")
	      (pprint exp1)
	      (format t "~%~%Final expansion:")
	      (pprint exp)))
     (format t "~%~%")
     (values)))

(defmacro set-nil (var)
  (list 'setq var nil))

(defmacro simple-incf (var)
  "using quotes"
  (list 'setq var (list '+ var 1)))

(defmacro simpler-incf (var &optional (amount 1))
  "using back-tick and , for unquote"
  `(setq ,var (+ ,var ,amount)))

(defmacro simple-rotatef (a b)
  `(let ((c ,a)
	 (d ,b))
     (setq ,a d)
     (setq ,b c)))

(defmacro set-mutual (a b)
  `(progn
    (setq ,a 'b)
    (setq ,b 'a)))

; my version
(defmacro variable-chain (&rest variables)
  `(cond
     ((null '(,@variables))
      nil)
     ((null (cdr '(,@variables)))
      nil)
     ((null (cadr '(,@variables)))
      nil)
     (t (defvar ,(car variables) ',(cadr `,variables))
	(variable-chain (cdr '(,@variables))))))

; book version
(defmacro variable-chain-book (&rest vars)
  `(progn ; why is this necessary?!? because without it you'd have (,@do and get "illegal functiona call"
     ,@(do ((v vars (rest v)) ; set v; how to increment v
	    (res nil)) ; create variable res
	   ((null (rest v)) (reverse res)) ; end condition + consequence which is returned
	 (push `(setf ,(first v) ',(second v)) res)))) ; the ', is because we're assigning the symbol as the value
; this is supposed to expand to (setf a = 'b)

; &aux is like the lambda-list version of let*
(defun average (&rest args
		&aux (len (length args))) ; args is defined earlier in the lambda-list
  (/ (reduce #'+ args) len 1.0))
