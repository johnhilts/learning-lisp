(ql:quickload :cl-ppcre)
(ql:quickload :drakma)

(defparameter *base-url* "http://gigamonkeys.com/book/")
(defparameter *pcl1* (drakma:http-request *base-url*))

(defun scrape ()
  (do* ((links nil)
        (x 0 (+ 1 x))
        (start 0))
       ((or (null start) (> x 100)) (reverse links))
    (multiple-value-bind
          (next-start end)
        (cl-ppcre:scan "[\\w_-]+\\.html" *pcl1* :start start)
      (setf start end)
      (unless (null next-start)
        (let ((format-link
               #'(lambda ()
                   (let ((page (subseq *pcl1* next-start end)))
                     (format nil "~a~a" *base-url* page)))))
          (push (funcall format-link) links))))))
