(defstruct node
  (name)
  (question)
  (yes-case)
  (no-case))

(defvar *node-list* nil)

(defun init ()
  (setf *node-list* nil))
