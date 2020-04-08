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
