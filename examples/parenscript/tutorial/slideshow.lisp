;; (ql:quickload '(cl-who hunchentoot parenscript cl-ppcre cl-fad))
(mapc #'ql:quickload '(:cl-fad :cl-who :hunchentoot :parenscript :cl-ppcre))

(defpackage :slideshow-example
  (:use :cl :cl-who :hunchentoot :parenscript :cl-ppcre :cl-fad))

(in-package :slideshow-example)

(setq cl-who:*attribute-quote-char* #\")

(defun start-server (port)
  (start (make-instance 'easy-acceptor :port port)))

(defparameter *my-http-server* (start-server 5050))

(defun stop-server (server)
  (stop server))

(defvar *slideshows* (make-hash-table :test 'equalp))

(defun add-slideshow (slideshow-name image-folder)
  (setf (gethash slideshow-name *slideshows*) image-folder)
  (push (create-folder-dispatcher-and-handler
         (format nil "/slideshow-images/~a/" slideshow-name)
         image-folder)
        *dispatch-table*))

(add-slideshow "lolcat" "/home/jfh/Pictures/")
(add-slideshow "lolrus" "/home/jfh/code/csharp/")

(defmacro+ps slideshow-image-uri (slideshow-name image-file)
  `(concatenate 'string "/slideshow-images/" ,slideshow-name "/" ,image-file))

(defun slideshow-handler ()
  (cl-ppcre:register-groups-bind (slideshow-name)
      ("/slideshows/(.*)" (script-name*))
    (let* ((images (mapcar
                    (lambda (i) (url-encode (file-namestring i)))
                    (cl-fad:list-directory
                     (or (gethash slideshow-name *slideshows*)
                         (progn (setf (return-code*) 404)
                                (return-from slideshow-handler))))))
           (current-image-index
            (or (position (url-encode (or (get-parameter "image") ""))
                          images
                          :test #'equalp)
                0))
           (previous-image-index (max 0
                                      (1- current-image-index)))
           (next-image-index (min (1- (length images))
                                  (1+ current-image-index))))
      (with-html-output-to-string (s)
        (:html
         (:head
          (:title "Parenscript slideshow")
          (:script
           :type "text/javascript"
           (str
            (ps*
             `(progn
                (var *slideshow-name* ,slideshow-name)
                (var *images* (array ,@images))
                (var *current-image-index* ,current-image-index)))))
          (:script :type "text/javascript" :src "/slideshow.js"))
         (:body
          (:div :id "slideshow-container"
                :style "width:100%;text-align:center"
                (:img :id "slideshow-img-object"
                      :src (slideshow-image-uri
                            slideshow-name
                            (elt images current-image-index)))
                :br
                (:a :href (format nil "/slideshows/~a?image=~a"
                                  slideshow-name
                                  (elt images previous-image-index))
                    :onclick (ps (previous-image) (return false))
                    "Previous")
                " "
                (:a :href (format nil "/slideshows/~a?image=~a"
                                  slideshow-name
                                  (elt images next-image-index))
                    :onclick (ps (next-image) (return false))
                    "Next"))))))))


(push (create-prefix-dispatcher "/slideshows/" 'slideshow-handler)
      *dispatch-table*)

(define-easy-handler (js-slideshow :uri "/slideshow.js") ()
  (setf (content-type*) "text/javascript")
  (ps
    (define-symbol-macro fragment-identifier (@ window location hash))

    (defun show-image-number (image-index)
      (let ((image-name (aref *images* (setf *current-image-index* image-index))))
        (setf (chain document (get-element-by-id "slideshow-img-object") src)
              (slideshow-image-uri *slideshow-name* image-name)
              fragment-identifier
              image-name)))

    (defun previous-image ()
      (when (> *current-image-index* 0)
        (show-image-number (1- *current-image-index*))))

    (defun next-image ()
      (when (< *current-image-index* (1- (getprop *images* 'length)))
        (show-image-number (1+ *current-image-index*))))

    ;; use fragment identifiers to allow bookmarking
    (setf (getprop window 'onload)
          (lambda ()
            (when fragment-identifier
              (let ((image-name (chain fragment-identifier (slice 1))))
                (dotimes (i (length *images*))
                  (when (string= image-name (aref *images* i))
                    (show-image-number i)))))))))
