(defpackage #:test-def-global
  (:use #:cl))

(in-package #:test-def-global)

(sb-ext:defglobal *not-defvar* 123)

(defconstant +not-def-global+ 456)

(sb-ext:defglobal *un-assigned* nil)

(defconstant +un-assigned-const+ nil)

(defun do-assignments ()
  (setf *un-assigned* 789)
  ;; (setf +un-assigned-const+ 111)  ;; error: +UN-ASSIGNED-CONST+ is a constant and thus can't be set.
  )

(defun do-re-assignments ()
  (setf *not-defvar* 222)
  ;;  (setf +not-def-global+ 333)  ;; error: +NOT-DEF-GLOBAL+ is a constant and thus can't be set.
  )

(defun add1 (number)
  (1+ number))

;; Compile-time error:
;;   TEST-DEF-GLOBAL::*NOT-DEFVAR* names a global lexical variable, and cannot be
;; used in an ordinary lambda list.
;; (defun test-global-param (*not-defvar*)
;;   (1+ *not-defvar*))

;; Compile-time error:
;;   TEST-DEF-GLOBAL::+NOT-DEF-GLOBAL+ names a defined constant, and cannot be used
;; in an ordinary lambda list.
;; (defun test-constant-param (+not-def-global+)
;;   (1+ +not-def-global+))

(do-assignments)
(do-re-assignments)

(print (add1 *not-defvar*))
(print (add1 +not-def-global+))
(print (add1 *un-assigned*))

;; The value
;;   NIL
;; is not of type
;;   NUMBER
;; when binding SB-KERNEL::X
;;    [Condition of type TYPE-ERROR]

;; (add1 +un-assigned-const+)
