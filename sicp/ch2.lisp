(defpackage #:sicp-repl
  (:use #:cl))

;; (in-package #:sicp-repl)

;; 2.4
(defun cons-sicp (x y)
  #'(lambda (m) (funcall m x y)))

(defun car-sicp (z)
  (funcall z #'(lambda (p q) p)))

(defun cdr-sicp (z)
  (funcall z #'(lambda (p q) q)))


