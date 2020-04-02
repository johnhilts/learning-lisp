(defstruct node
  (name)
  (question)
  (yes-case)
  (no-case))

(defvar *node-list* nil)

(defun init ()
  (setf *node-list* nil))

(defun add-node (name question yes-case no-case)
  (setf node (make-node :name name
			:question question
			:yes-case yes-case
			:no-case no-case))
  (push node *node-list*)
  (node-name node))
; it's also possible to push directly onto the list
