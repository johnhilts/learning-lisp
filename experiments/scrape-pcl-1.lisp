(ql:quickload :cl-ppcre)
(ql:quickload :drakma)

(defparameter *base-url* "http://gigamonkeys.com/book/")
(defparameter *pcl1* (drakma:http-request *base-url*))

(defun get-app-dir ()
  (or
   (cadr sb-ext:*posix-argv*)
   (directory-namestring (user-homedir-pathname))))

(defun get-link-filename ()
  (format nil "~a/~a" (get-app-dir) "my-links.txt"))

(defun get-links (start pattern)
  "scan downloaded HTML page for all links"
  (multiple-value-bind
        (start end)
      (cl-ppcre:scan pattern *pcl1* :start start)
    (unless (null start)
      (let ((format-link #'(lambda () (subseq *pcl1* start end))))
        (append (list (funcall format-link)) (get-links end pattern))))))

(defun build-url (link)
  "combine base URL with relative path link"
  (format nil "~a~a" *base-url* link))

(defun fetch-html (link)
  "fetch html for given relative path (will combine with base URL)"
  (let ((html-file (format nil "~a/~a" (get-app-dir) link))
        (html (drakma:http-request (build-url link))))
    (with-open-file
        (stream html-file :direction :output :if-exists :supersede)
      (format stream "~a" html))))

(defun scrape ()
  "entry point"
  
  (format t "~&PCL Scraper v0.1~%Using directory: ~a" (get-app-dir))
  (format t "~&my-links.txt full path: ~a" (get-link-filename))
  
  (let* ((pattern (ppcre:create-scanner "[\\w_-]+\\.html"))
          (links (get-links 0 pattern)))
    (mapcar #'fetch-html links)
    (with-open-file
        (stream (get-link-filename) :direction :output :if-exists :supersede)
      (format stream "~{~&~a~}" (mapcar #'build-url links)))))

