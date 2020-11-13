;; (ql:quickload :cl-ppcre)
;; (ql:quickload :drakma)

(defpackage #:scrape-parenscript-mailer
  (:use #:cl #:cl-ppcre #:drakma)
  (:export :scrape))

(in-package #:scrape-parenscript-mailer)

(defparameter *base-url* "https://mailman.common-lisp.net/pipermail/parenscript-devel/")
(defparameter *pcl1* (drakma:http-request *base-url*))

(defun get-app-dir ()
  (or
   (cadr sb-ext:*posix-argv*)
   (directory-namestring (user-homedir-pathname))))

(defun get-link-filename ()
  (format nil "~a/~a" (get-app-dir) "my-parenscript-mailinglist-links.txt"))

(defun get-links (start pattern)
  "scan downloaded HTML page for all links"
  (multiple-value-bind
        (start end)
      (cl-ppcre:scan pattern *pcl1* :start start)
    (unless (null start)
      (let ((format-link #'(lambda () (subseq *pcl1* (+ 1 start) end))))
        (append (list (funcall format-link)) (get-links end pattern))))))

(defun build-url (link)
  "combine base URL with relative path link"
  (format nil "~a~a" *base-url* link))

(defun string-replace (string search replace)
  (labels
      ((replace-r (string search replace)
         (if (zerop (length string))
             string
             (let ((search-position (search search string)))
               (if (null search-position)
                   string
                   (concatenate 'string
                                (subseq string 0 search-position)
                                replace
                                (replace-r (subseq string  (+ search-position (length search))) search replace)))))))
    (replace-r string search replace)))

(defun fetch-html (link)
  "fetch html for given relative path (will combine with base URL)"
  (let ((html-file (format nil "~a/~a" (get-app-dir) (string-replace link "/" "-")))
        (html (drakma:http-request (build-url link))))
    (with-open-file
        (stream html-file :direction :output :if-exists :supersede)
      (format stream "~a" html))))

(defun scrape ()
  "entry point"
  
  (format t "~&Parenscript Mailing List Archive Scraper v0.1~%Using directory: ~a" (get-app-dir))
  (format t "~&my-parenscript-mailinglist-links.txt full path: ~a" (get-link-filename))
  
  (let* ((pattern (ppcre:create-scanner "\"[\\w/-]+\\.html"))
          (links (get-links 0 pattern)))
    (mapcar #'fetch-html links)
    (with-open-file
        (stream (get-link-filename) :direction :output :if-exists :supersede)
      (format stream "~{~&~a~}~%" (mapcar #'build-url links)))))

