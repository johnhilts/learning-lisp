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

(defun repeated (f n)
  (cond
    ((= n 1)
     #'(lambda (x)
         (funcall f x)))
    (t (compose ;; remember, compose just takes 2 functions, that's it!
        f
        (repeated f (- n 1))))))
;; (funcall (repeated #'square 2) 5)
;; => 625

(defun gcd-euclid (a b)
  "GCD using Euclid's algorithm"
  (if (= b 0)
      a
      (gcd b (mod a b))))

(defun gcd-euclid-loop (a b)
  "GCD using Euclid's algorithm, using a loop"
  (do
   ((remainder))
   ((= b 0) a)
    (setf remainder (mod a b))
    (setf a b)
    (setf b remainder)))

(defun repeated (f n)
  (cond
    ((= n 1)
     (lambda (x)
       (funcall f x)))
    (t (compose ;; remember, compose just takes 2 functions, that's it!
           f
           (repeated f (- n 1))))))

(defparameter *tolerance* 0.00001)

(defun fixed-point (f first-guess)
  (flet ((close-enough? (v1 v2)
           (< (abs (- v1 v2)) *tolerance*)))
    (labels ((try (guess counter)
               (let ((next (funcall f guess)))
                 (princ counter)
                 (princ " : ")
                 (princ next)
                 (terpri)
                 (if (close-enough? guess next)
                     next
                     (try next (+ 1 counter))))))
      (terpri)
      (try first-guess 1))))

(defun average (x y)
  (/ (+ x y) 2))

(defun average-damp (f)
  (lambda (x)
    (average x (funcall f x))))

(defparameter *dx* 0.00001)

(defun deriv (g)
  (lambda (x)
    (/ (- (funcall g (+ x *dx*)) (funcall g x))
       *dx*)))

(defun cube (x) (* x x x))

(funcall (deriv #'cube) 5) ; run this

(defun newton-transform (g)
  (lambda (x)
    (- x (/ (funcall g x)
            (funcall (deriv g) x)))))

(defun newtons-method (g guess)
  (fixed-point (newton-transform g)
               guess))

;; various version of sqrt ... I probably haven't captured all of them
(defun sqrt-newton (x)
  (newtons-method
   (lambda (y)
     (- (square y) x))
   1.0))

(defun fixed-point-of-transform (g transform guess)
  (fixed-point (funcall transform g) guess)) ; fixed-point defined 1.36

(defun sqrt-fpt (x)
  (fixed-point-of-transform
   (lambda (y) (/ x y))
   #'average-damp
   1.0))

(defun sqrt-fp (x)
  (fixed-point (lambda (y) (/ x y))
               1.0))

(defun cube-root (x)
  (fixed-point 
   (average-damp 
    (lambda (y) 
      (/ x (square y))))
   1.0))

(defun cube-root-fpt (x)
  (fixed-point-of-transform
   (lambda (y) (/ x (square y)))
   #'average-damp
   1.0))

(defun fourth-root-non-converging (x)
  "This doesn't work and will result in an infinite loop"
  (fixed-point 
   (average-damp 
    (lambda (y) 
      (/ x (cube y))))
   1.0))

(defun fourth-root-converging (x)
  "This is supposed to work because it average damps twice"
  (fixed-point 
   (average-damp
    (average-damp 
     (lambda (y) 
       (/ x (cube y)))))
   1.0))

;; expirement results
;; 4th root with 1 avg dam is is infinite loop
;; 7th root with 2 avg damps is inifite loop
(defun get-root (x power)
  (fixed-point 
   (average-damp
    (average-damp
     (average-damp 
      (lambda (y) 
        (/ x (expt y (- power 1)))))))
   1.0))

(defun get-root-on-repeat (x power)
  (let ((count (cond
                 ((< power 4)
                  1)
                 ((< power 7)
                  2)
                 (t 3))))
    (fixed-point (funcall (repeated #'average-damp count)
                          (lambda (y) (/ x (expt y (- power 1)))))
                 1.0)))


;; (fixed-point (lambda (x) (/ (log 1000) (log x))) 10.0)
;; => 33 times
;; (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 10.0)
;; => 10 times

;; (funcall (repeated #'square 2) 5)
;; => 625

;; (funcall (compose #'square #'inc) 6)
;; (funcall (repeated #'square 2) 5)
