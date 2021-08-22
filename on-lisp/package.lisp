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


