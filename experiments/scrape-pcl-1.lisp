(ql:quickload :cl-ppcre)
(ql:quickload :drakma)

(defparameter *base-url* "http://gigamonkeys.com/book/")
(defparameter *pcl1* (drakma:http-request *base-url*))
(defparameter *app-dir* "/home/jfh/code/lisp/source/learning-lisp/experiments/")
(defparameter *link-file* (format nil "~a~a" *app-dir* "my-links.txt"))

(defun get-links (start pattern)
  (multiple-value-bind
        (start end)
      (cl-ppcre:scan pattern *pcl1* :start start)
    (unless (null start)
      (let ((format-link #'(lambda () (subseq *pcl1* start end))))
        (append (list (funcall format-link)) (get-links end pattern))))))

(defun fetch-html (link)
  (let ((html-file (format nil "~a~a" *app-dir* link))
        (html (drakma:http-request (build-url link))))
    (with-open-file
        (stream html-file :direction :output :if-exists :supersede)
      (format stream "~a" html))))

(defun build-url (link)
  (format nil "~a~a" *base-url* link))

(defun scrape ()
  (let* ((pattern (ppcre:create-scanner "[\\w_-]+\\.html"))
         (links (get-links 0 pattern)))
    (mapcar #'fetch-html links)
    (with-open-file
        (stream *link-file* :direction :output :if-exists :supersede)
      (format stream "~{~&~a~}" (mapcar #'build-url links)))))

