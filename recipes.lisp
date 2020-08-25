;; 1-2
;; simple gensym example
(defmacro swap (var-1 var-2)
  (let ((temp-var (gensym))) ;; <- added
    `(let ((,temp-var ,var-1)) ;; <- changed
       (setf ,var-1 ,var-2
             ,var-2 ,temp-var) ;; <- changed
       (values))))
