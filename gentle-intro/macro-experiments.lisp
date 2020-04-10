#||
What's the difference between ` ...,variable and `...,@?
I want to see what the difference is by switching them around in some macros I found online.
||#

; original version
(defmacro simple-rotatef (a b)
  `(let ((c ,a)
	 (d ,b))
     (setq ,a d)
     (setq ,b c)))

; different quoting style
(defmacro simple-rotatef-with-list (a b)
  (list 'let (list (list 'c a)
	 (list 'd b))
     (list 'setq a 'd)
     (list 'setq b 'c)))
; this works

; original version
(defmacro letx (var val &body body)
  `(let ((,var ,val)) ,@body))

; different quoting style
(defmacro letx-with-list (var val &body body)
  (list 'let (list (list var val)) body)) ; body gets expanded to ((expression)), not sure why

; don't use ,@, just use ,
(defmacro letx-no-@ (var val &body body)
  `(let ((,var ,val)) ,body)) ; same thing here - body gets expanded to ((expression)) WHY??
;;; maybe it's because "&body" or "&rest" means "this is a list" and then a form is inside there, so
;;; then you get the doubled up parentheses

;;; ... if something's a list, does that mean you have to "splice" it?
; an append macro
(defmacro my-append (list1 list2)
  `(append ,list1 ,list2))
;;; usage: (my-append '(1 2 3) '(4 5 6))
;;; also, I thought I needed ,@list1 but that didn't expand correctly


;;; ... if something's a &rest param, does that mean you have to "splice" it?
; and append macro
(defmacro my-append-with-rest (&rest list)
  `(append ,@list))
;;; usage: (my-append '(1 2 3) '(4 5 6))
;;; This time, I needed ,@list1

#||
So ... maybe if it's a quoted list (hard-coded list), use `...,
If it's a generated list - such as from a &rest param, or a (mapcar), or even (do), then use `...,@

ok, I found something that explained the difference clearly!!
`..., will get the value of whatever it's unquoting!
`...,@ will get the ELEMENTS of a list!
example using a list:
||#

(defmacro my-cool-macro (&rest list)
  `(progn
    (format t "~s" (append '(1 2 3) ,@list)) ; don't use IO in normal macros!
    (setf list-from-a-macro ,@list)))

(defvar my-example-list '(1 2 3))
(my-cool-macro my-example-list)

