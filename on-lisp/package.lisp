(defpackage #:on-lisp-repl
  (:use #:cl))

;; (in-package #:on-lisp-repl)

(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))

(funcall (compose #'1+ #'find-if) #'oddp '(2 3 4))

(defmacro before-wrong (x y seq)
  `(let ((seq ,seq))
     (< (position ,x seq)
        (position ,y seq))))

(defmacro before-right (x y seq)
  `(let ((xval ,x) (yval ,y) (seq ,seq))
     (< (position xval seq)
        (position yval seq))))

;; example using the before macro
(before-wrong (progn (setq seq '(b a)) 'a)
              'b
              '(a b))

(let ((seq))
  (before-right (progn (setq seq '(b a)) 'a)
                'b
                '(a b)))

(defun compose (&rest fns)
  "1. take a list of functions
   2. arrange them the way you want - call them in order from the last to the first
   3. then return a lambda that does 1 & 2
   4. the lambda will take whatever arguments are given to the returned function"
  (if fns
      (let ((fn1 (car (last fns)))
	    (fns (butlast fns)))
	#'(lambda (&rest args)
	    (reduce #'funcall fns
		      :from-end t
		      :initial-value (apply fn1 args))))
      #'identity))


(defun jfh-compose (&rest functions)
  (format t "functions count: ~d~%" (length functions))
  #'(lambda (&rest args)
      (format t "arg count: ~d~%" (length args))))

(defun signed (doc)
  (format t "~&doc = ~a~%" doc)
  (cond
    ((string= "doc1" doc) t)
    ((string= "doc3" doc) t)
    (t nil)))

(defun sealed (doc)
  (format t "~&doc = ~a~%" doc)
  (cond
    ((string= "doc2" doc) t)
    ((string= "doc3" doc) t)
    (t nil)))

(defun delivered (doc)
  (format t "~&doc = ~a~%" doc)
  (cond
    ((string= "doc2" doc) t)
    ((string= "doc3" doc) t)
    (t nil)))

(defun fint (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
	#'(lambda (x)
	    (and (funcall fn x) (funcall chain x))))))

(defun lrec (rec &optional base)
  "list recursor"
  (labels ((self (lst)
	     (if (null lst)
		 (if (functionp base)
		     (funcall base)
		     base)
		 (funcall rec (car lst)
			  #'(lambda ()
			      (self (cdr lst)))))))
    #'self))
