(defpackage #:on-lisp-repl
  (:use #:cl))

;; (in-package #:on-lisp-repl)

(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))

(funcall (compose #'1+ #'find-if) #'oddp '(2 3 4))

(defmacro before-wrong (x y seq)
  `(let ((seq ,seq))
     (< (position ,x seq)
        (position ,y seq))))

(defmacro before-right (x y seq)
  `(let ((xval ,x) (yval ,y) (seq ,seq))
     (< (position xval seq)
        (position yval seq))))

;; example using the before macro
(before-wrong (progn (setq seq '(b a)) 'a)
              'b
              '(a b))

(let ((seq))
  (before-right (progn (setq seq '(b a)) 'a)
                'b
                '(a b)))

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))
;; example - note parenthesis around 1st 2 arguments
(when-bind (x 4)
  (+ x 3))

(defmacro when-bind2 (var expr &body body) ; <-- removed parenthesis here
  `(let ((,var ,expr))
     (when ,var
       ,@body)))
;; example - note lack of parenthesis around 1st 2 arguments
(when-bind2 x 4
  (+ x 3))

(defmacro in-if (fn &rest choices)
  (let ((fnsym (gensym)))
    `(let ((,fnsym ,fn))
       (or ,@(mapcar #'(lambda (c)
                         `(funcall ,fnsym ,c))
                     choices)))))

(defmacro in-if-bad? (fn &rest choices)
  `(or ,@(mapcar #'(lambda (c)
                     `(funcall ,fn ,c))
                 choices)))

(defun stop-fn (x stop)
  (and (oddp x) (> x stop)))

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

(defmacro for-bad ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var))
        (limit ,stop))
       ((> ,var limit))
     ,@body))

(defmacro for-test ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var)))
       ((> ,var ,stop))
     ,@body))

;; get exported symbol info
(LET ((count 0)
      (external-symbols ()))
  (DO-EXTERNAL-SYMBOLS (THIS 'CL count)
    (INCF count)
    (push this external-symbols))
  (format t "~&count: ~d~%Some external symbols: ~%~a~%" count (mapcar (lambda (e) (format nil "~a~%" e)) (subseq external-symbols 0 10))))

;; tiny try/catch style wrapping 
(mapcar (lambda (arg) (handler-case (symbol-name arg) (type-error (err) err))) '(1 deux "trois" :quatre))

(defun signed (doc)
  (format t "~a signed!~%" doc)
  t)

(defun sealed (doc)
  (format t "~a sealed!~%" doc)
  t)

(defun delivered (doc)
  (format t "~a delivered!~%" doc)
  t)

(defun fint (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
        #'(lambda (x)
            (and (funcall fn x) (funcall chain x))))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defmacro with-struct ((name . fields) struct &body body)
  (let ((gs (gensym)))
    `(let ((,gs ,struct))
       (let ,(mapcar #'(lambda (f)
                         `(,f (,(symb name f) ,gs)))
                       fields)
         ,@body))))

;; examples
(defstruct visitor name title firm)
(setq theo (make-visitor :name "Theodebert"
                         :title 'king
                         :firm 'franks))

(with-struct (visitor- name firm title) theo
  (list name firm title))

(defun destruc (pat seq &optional (atom? #'atom) (n 0))
  (if (null pat)
      nil
      (let ((rest (cond ((funcall atom? pat) pat)
                        ((eq (car pat) '&rest) (cadr pat))
                        ((eq (car pat) '&body) (cadr pat))
                        (t nil))))
        (if rest
            `((,rest (subseq ,seq ,n)))
             (let ((p (car pat))
                   (rec (destruc (cdr pat) seq atom? (1+ n))))
               (if (funcall atom? p)
                   (cons `(,p (elt ,seq ,n))
                          rec)
                   (let ((var (gensym)))
                     (cons (cons `(,var (elt ,seq ,n))
                                  (destruc p var atom?))
                           rec))))))))

(defun wplac-ex (binds body)
  (if (null binds)
      `(progn ,@body)
       `(symbol-macrolet ,(mapcar #'(lambda (b)
                                      (if (consp (car b))
                                          (car b)
                                          b))
                                    binds)
          ,(wplac-ex (mapcan #'(lambda (b)
                                 (if (consp (car b))
                                     (cdr b)))
                               binds)
                     body))))

(defmacro with-places (pat seq &body body)
  (let ((gseq (gensym)))
    `(let ((,gseq ,seq))
       ,(wplac-ex (destruc pat gseq #'atom) body))))

;; examples
(let ((lst '(1 (2 3) 4)))
  (with-places (a (b . c) d) lst
               (setf a 'uno)
               (setf c '(tre)))
  lst)

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro acond2 (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (val (gensym))
            (win (gensym)))
        `(multiple-value-bind (,val ,win) ,(car cl1)
           (if (or ,val ,win)
               (let ((it ,val)) ,@(cdr cl1))
               (acond2 ,@(cdr clauses)))))))

(defun varsym? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

(defun binding (x binds)
  (labels ((recbind (x binds)
             (aif (assoc x binds)
                  (or (recbind (cdr it) binds)
                      it))))
    (let ((b (recbind x binds)))
      (values (cdr b) b))))

(defun match (x y &optional binds)
  (acond2
   ((or (eql x y) (eql x '_) (eql y '_)) (values binds t))
   ((binding x binds) (match it y binds))
   ((binding y binds) (match x it binds))
   ((varsym? x) (values (cons (cons x y) binds) t))
   ((varsym? y) (values (cons (cons y x) binds) t))
   ((and (consp x) (consp y) (match (car x) (car y) binds))
    (match (cdr x) (cdr y) it))
   (t (values nil nil))))

;; examples
(match '(p a b c a) '(p ?x ?y c ?x))
#|
;; note: multi-value result
((?Y . B) (?X . A))
T
|#

(defun make-db (&optional (size 100))
  (make-hash-table :size size))

(defvar *default-db* (make-db))

(defmacro db-query (key &optional (db '*default-db*))
  `(gethash ,key ,db))
