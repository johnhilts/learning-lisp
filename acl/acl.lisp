(defpackage #:acl-repl
  (:use #:cl))

;; (in-package #:acl-repl)

(defun length/r (1st)
  (if (null 1st)
      0
      (1+ (length/r (cdr 1st)))))

(defun length/tr (1st)
  (labels ((len (1st ace)
             (if (null 1st)
                 ace
                 (len (cdr 1st) (1+ ace)))))
    (len 1st 0)))
