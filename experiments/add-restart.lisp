(define-condition first-not-number (error)
  ((message :initarg :message :reader error-message)))

(define-condition second-not-number (error)
  ((message :initarg :message :reader error-message)))


(defun get-new-value (param)
  (format *query-io* "Enter a new value for ~s: " param)
  (force-output *query-io*)
  (list (read)))

(defun add (x y)
  (restart-case (cond ((and (realp x) (realp y))
                       (+ x y))
                      ((not (realp x))
                       (error 'first-not-number
                              :message "param 1 not a number"))
                      ((not (realp y))
                       (error 'second-not-number
                              :message "param 2 not a number")))
    (return-zero () 0)
    (return-random-value () (random 100))
    (restart-with-new-first (x)
      :report "Supply a new value for first param"
      :interactive (lambda () (get-new-value 'x))
      (add x y))
    (restart-with-new-second (y)
      :report "Supply a new value for second param"
      :interactive (lambda () (get-new-value 'y))
      (add x y))
    (just-continue () nil)))


(defun add-client ()
  (let ((x nil)
        (y nil))
    (princ "Enter the first number: ")
    (setf x (read))
    (princ "Enter the second number: ")
    (setf y (read))
    (handler-bind
        ((first-not-number (lambda (c)
                             (format t "error: ~s~%" (error-message c))
                             (invoke-restart 'restart-with-new-first)))
         (second-not-number (lambda (c)
                              (format t "error: ~s~%" (error-message c))
                              (invoke-restart 'restart-with-new-second))))
                  (add x y))))
