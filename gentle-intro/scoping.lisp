(defvar birds)
(setf fish '(salmon tuna))
(setf birds '(eagle vulture))
(defun ref-fish ()
  fish)
(defun ref-birds ()
  birds)
(defun test-lexical (fish)
  (list fish (ref-fish)))
(test-lexical '(guppy minnow))
(defun test-dynamic (birds)
  (list birds (ref-birds)))
(test-dynamic '(robin sparrow))
; returns ((ROBIN SPARROW) (ROBIN SPARROW))
(ref-birds) ; returns (EAGLE VULTURE)

#||
the test-dynamic function illustrates how Lisp's dynamic scoping works.
"birds" is a global variable declared with defvar, making it a dynamically scoped variable.
But inside test-dynamic, a local variable is declared that overrides it. Even when calling another function.
"fish", which wasn't declared with defvar, works the "normal" way (lexical scoping).
Why would you ever use defvar?
Why do global variables declared with setf cause a warning in sbcl??
Note: dynamically scoped variables are also called "special variables"
||#
