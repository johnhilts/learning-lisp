(let* ((original-alist '(("hello" . 42) ("world" . 95)))
       (new-alist
        (mapcar
         (lambda (x)
           (destructuring-bind
                 (car . cdr) x
             (cons (intern (string-upcase car) :keyword) cdr)))
         original-alist)))
  new-alist)

(defmacro passthrough+ (&rest rest)
  (labels ((process (rest)
             (cond
               ((null rest)
                0)
               ((atom rest)
                `(eval ,rest))
               (t
                `(eval ,(cadr rest))))))
    `(+ ,@(mapcar #'process rest))))
