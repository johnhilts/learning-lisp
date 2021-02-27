;;; write COPY-LIST and REVERSE for lists using reduce
;;; COPY-LIST returns a new copy of a list
;;; (EQUAL list (COPY-LIST list)) => T
;;; (EQL list (COPY-LIST list)) => NIL, unless list itself is NIL
(in-package #:acl-exercises)

(defun copy-list-with-reduce (list)
  (reverse ;; the element order has to match for EQUAL to work
   (reduce
    #'(lambda (acc cur)
        (cons cur acc))
    list :initial-value ())))

(defun test-copy-list ()
  (let* ((old-list (list 'one 'two 'three))
         (new-list (copy-list-with-reduce old-list)))
    (format t "equal should be t: ~a~%" (equal old-list new-list))
    (format t "eql should be nil: ~a~%" (eql old-list new-list))))

(test-copy-list)
