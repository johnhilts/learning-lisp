(defpackage #:sicp-repl
  (:use #:cl))

;; (in-package #:sicp-repl)

(defun generate-pascals-triangle (&optional (levels-deep 5))
  "Generate Pascal's Triangle n levels deep Pascal's Triangle has all
1s on the edges, and each node is calculated by its 2 parents.  Use 0
if a parent is missing."
  (labels ((generate-line (previous-line)
           (let ((previous-node 0))
             (append
              (mapcar #'(lambda (e)
                          (let ((sum (+ previous-node e)))
                            (setf previous-node e)
                            sum))
                      previous-line)
              (list 1))))
         (generate-triangle ()
           (do
            ((i 0 (incf i))
             (triangle (list (list 1))))
            ((> i levels-deep) (reverse triangle))
             (push (generate-line (car triangle)) triangle))))
    (let ((triangle (generate-triangle)))
      (mapc
       #'(lambda (e)
           (format t "~{ ~a ~}~%" e))
       triangle))))

;; some additional thoughts:
;; - don't use any zeroes!
;; - just have the actual visible numbers
;; - we can get the (car) and (last (car)) of each "line" and figure out whether to use zero
;; - maybe do something like: 1st and last are always 1
;; therefore, just grab everything (butfirst) and (butlast)  (intersection (cdr) (butlast))
;;
;;     1
;;    1  1
;;   1 2  1
;; 1  3  3 1
;;1 4  6  4 1

(defun inc (n)
  (incf n))

(defun square (n) (* n n))

(defun compose (f g)
  (lambda (x) (funcall f (funcall g x))))

;; (funcall (compose #'square #'inc) 6)
;; => 49
;; note: I hadn't realized that it was necessary to use #'funcall on a lambda!!
