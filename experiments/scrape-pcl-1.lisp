(ql:quickload :cl-ppcre)
(ql:quickload :drakma)

(defparameter *base-url* "http://gigamonkeys.com/book/")
(defparameter *pcl1* (drakma:http-request *base-url*))
(defparameter *link-file* "/home/jfh/code/lisp/source/learning-lisp/experiments/my-links.txt")

(defun get-links (start pattern)
  (multiple-value-bind
        (start end)
      (cl-ppcre:scan pattern *pcl1* :start start)
    (unless (null start)
      (let ((format-link
             #'(lambda ()
                 (let ((page (subseq *pcl1* start end)))
                   (format nil "~a~a" *base-url* page)))))
        (append (list (funcall format-link)) (get-links end pattern))))))

(defun scrape ()
  (let* ((pattern (ppcre:create-scanner "[\\w_-]+\\.html"))
         (links (get-links 0 pattern)))
    (with-open-file (stream *link-file* :direction :output :if-exists :supersede)
      (format stream "~{~&~a~}" links))))
