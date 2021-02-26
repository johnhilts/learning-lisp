;;; write COPY-LIST and REVERSE for lists using reduce
;;; COPY-LIST returns a new copy of a list
;;; (EQUAL list (COPY-LIST list)) => T
;;; (EQL list (COPY-LIST list)) => NIL, unless list itself is NIL
(in-package #:acl-exercises)

(defun reverse-with-reduce (list)
  "return new reversed list"
  (reduce
   #'(lambda (acc cur)
       (cons cur acc))
   list :initial-value ()))

(defun test-reduce ()
  "test with real REVERSE"
  (let* ((old-list (list 'one 'two 'three))
         (new-list (reverse-with-reduce old-list)))
    (print "testing reduce ...")
    (terpri)
    (format t "equal should be t: ~a~%" (equal old-list (reverse new-list)))
    (format t "eql should be nil: ~a~%" (eql old-list (reverse new-list)))))

(test-reduce)

(defun copy-list-with-reduce (list)
  "create new list with same elements, using custom REVERSE"
  (reverse-with-reduce ;; the element order has to match for EQUAL to work
   (reduce
    #'(lambda (acc cur)
        (cons cur acc))
    list :initial-value ())))

(defun test-copy-list ()
  (let* ((old-list (list 'one 'two 'three))
         (new-list (copy-list-with-reduce old-list)))
    (print "testing copy-list ...")
    (terpri)
    (format t "equal should be t: ~a~%" (equal old-list new-list))
    (format t "eql should be nil: ~a~%" (eql old-list new-list))))

(test-copy-list)

