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

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))
;; example - note parenthesis around 1st 2 arguments
(when-bind (x 4)
  (+ x 3))

(defmacro when-bind2 (var expr &body body) ; <-- removed parenthesis here
  `(let ((,var ,expr))
     (when ,var
       ,@body)))
;; example - note lack of parenthesis around 1st 2 arguments
(when-bind2 x 4
  (+ x 3))

(defmacro in-if (fn &rest choices)
  (let ((fnsym (gensym)))
    `(let ((,fnsym ,fn))
       (or ,@(mapcar #'(lambda (c)
                         `(funcall ,fnsym ,c))
                     choices)))))

(defmacro in-if-bad? (fn &rest choices)
  `(or ,@(mapcar #'(lambda (c)
                     `(funcall ,fn ,c))
                 choices)))

(defun stop-fn (x stop)
  (and (oddp x) (> x stop)))

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

(defmacro for-bad ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var))
        (limit ,stop))
       ((> ,var limit))
     ,@body))

(defmacro for-test ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var)))
       ((> ,var ,stop))
     ,@body))

;; get exported symbol info
(LET ((count 0)
      (external-symbols ()))
  (DO-EXTERNAL-SYMBOLS (THIS 'CL count)
    (INCF count)
    (push this external-symbols))
  (format t "~&count: ~d~%Some external symbols: ~%~a~%" count (mapcar (lambda (e) (format nil "~a~%" e)) (subseq external-symbols 0 10))))

;; tiny try/catch style wrapping 
(mapcar (lambda (arg) (handler-case (symbol-name arg) (type-error (err) err))) '(1 deux "trois" :quatre))
