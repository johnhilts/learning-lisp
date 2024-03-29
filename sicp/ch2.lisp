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


(defun same-parity (x &rest y)
  (let ((parity (mod x 2)))
    (labels ((same-parity-r (y)
               (if (null (cdr y))
                   ()
                   (if (= parity (mod (car y) 2))
                       (append (list (car y)) (same-parity-r (cdr y)))
                       (same-parity-r (cdr y))))))
      (cons x (same-parity-r y)))))

;; (same-parity 1 2 3 4 5 6 7)
;; => (1 3 5 7)

;; (same-parity 2 3 4 5 6 7)
;; => (2 4 6)

(defun sicp-map (proc items)
  (if (null items)
      ()
      (cons (funcall proc (car items))
            (sicp-map proc (cdr items)))))

(defun square-list-1 (items)
  (if (null items)
      ()
      (cons (* (car items) (car items)) (square-list-1 (cdr items)))))

(defun square-list-2 (items)
    (sicp-map #'(lambda (e) (* e e)) items))

;; (square-list (list 1 2 3 4))
;; => (1 4 9 16)

(defun sicp-for-each (proc list)
  (cond
    ((null list)
     'done)
    (t
     (funcall proc (car list))
     (sicp-for-each proc (cdr list)))))

;; (for-each
;;  #'(lambda (x) (terpri) (princ x))
;;  (list 57 321 88))
;; 57
;; 321
;; 88

(defun sicp-reverse (list)
  (if (null list)
      ()
      (append (sicp-reverse (cdr list)) (list (car list)))))

;; (sicp-reverse (list 1 4 9 16 25))
;; => (25 16 9 4 1)

(defun deep-reverse (list)
  (cond
    ((null list)
     ())
    ((not (consp list))
     (list list))
    (t
     (append (deep-reverse (cdr list)) (if (consp (car list)) (list (deep-reverse (car list))) (deep-reverse (car list)))))))
;; (setf x (list (list 1 2) (list 3 4)))
;; (deep-reverse x)
;; => ((4 3) (2 1))

(defun fringe (items)
  "Takes a tree (represented as a list) as an argument and returns a list whose elements are all the leaves of the tree arranged in left-to-right order."
  (cond
   ((null items)
    '())
   ((not (consp items))
    (list items))
   (t
    (append
     (fringe (car items))
     (fringe (cdr items))))))

;; (setf 2-28 (list (list 1 2) (list 3 4)))

;; (fringe x)
;; => (1 2 3 4)

;; (fringe (list x x))
;; => (1 2 3 4 1 2 3 4)

;;  (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
;; => (1 (4 (9 16) 25) (36 49))

(defun square-tree-with-recursion (tree)
  "Squares all numbers in a tree while preserving the tree's structure."
  (cond
    ((null tree) ())
    ((not (consp tree))
     (* tree tree))
    (t
     (cons (square-tree-with-recursion (car tree)) (square-tree-with-recursion (cdr tree))))))

(defun square-tree-with-map (tree)
  "Squares all numbers in a tree while preserving the tree's structure."
  (sicp-map
   (lambda (sub-tree)
     (if  (consp sub-tree)
          (square-tree-with-map sub-tree)
          (* sub-tree sub-tree)))
   tree))

(defun tree-map (proc tree)
  "General-purpose tree mapping function."
  (sicp-map
   (lambda (sub-tree)
     (if (consp sub-tree)
          (tree-map proc sub-tree)
          (funcall proc sub-tree)))
   tree))

;;; The below should work
(defun square-it (item) (* item item))

(defun square-tree-generalized (tree)
  (tree-map #'square-it tree))

(defun subsets (s)
  (if (null s)
      (list ())
      (let ((rest (subsets (cdr s))))
        (append rest (sicp-map (lambda (item) (cons (car s) item)) rest)))))

;; (subsets (list 1 2 3))
;; => (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

(defun flatten-tree (tree)
  (cond
    ((null tree)
     tree)
    ((consp (car tree))
     (append (flatten-tree (car tree)) (flatten-tree (cdr tree))))
    (t tree)))

(defun flatten-tree-with-map (tree)
  (mapcar
   #'(lambda (e)
       (cond
         ((null e)
          e)
         ((consp e)
          (flatten-tree-with-map e))
         (t e)))
   tree))
