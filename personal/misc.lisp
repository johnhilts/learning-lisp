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

(defmacro define-namespace (namespace-name constructor)
  `(progn
     (defparameter ,namespace-name (make-hash-table :test (function eql)))
     (defun ,(intern (concatenate 'string (symbol-name 'find-) (symbol-name namespace-name))) (name)
       (gethash name ,namespace-name))
     (defun ,(intern (concatenate 'string (symbol-name 'ensure-) (symbol-name namespace-name)))
         (name &rest arguments &key &allow-other-keys)
       (or (gethash name ,namespace-name)
           (setf (gethash name ,namespace-name) (apply (function ,constructor) arguments))))
     ',namespace-name))

(defclass color ()
  ((red   :initarg :red   :reader color-red)
   (green :initarg :green :reader color-green)
   (blue  :initarg :blue  :reader color-blue)))

(define-namespace color
    (lambda (&rest arguments &key red green blue)
      (apply (function make-instance) 'color arguments)))

(ensure-color 'blue :blue 1.0)
(find-color 'blue)
;; --> #<color #x3020023FBCCD>
;; then you can define a macro such as (color-let ((green (0.1 1.0 0.2)))  (fill-rect (mix blue green)))

(defclass immutable-hash () ((immutable-hash-table :initarg :immutable-hash-table :reader immutable-hash-table)))
(defmethod get-ih (key (table immutable-hash))
  (gethash key (immutable-hash-table table)))
(defvar *table*
  (make-instance 'immutable-hash
                 :immutable-hash-table (let ((h (make-hash-table)))
                                         (setf (gethash :k1 h) 'v1 (gethash :k2 h) 'v2)
                                         h)))
