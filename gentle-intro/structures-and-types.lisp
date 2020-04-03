(defstruct node
  (name)
  (question)
  (yes-case)
  (no-case))

(defvar *node-list* nil)

(defun init ()
  (setf *node-list* nil))

(defun populate-nodes ()
  (add-node 'start
	    "Does the engine turn over?"
	    'engine-turns-over
	    'engine-wont-turn-over)
  (add-node 'engine-turns-over
	    "Will the engine run for any period of time?"
	    'engine-will-run-briefly
	    'engine-wont-run)
  (add-node 'engine-wont-run
	    "Is there gas in the tank?"
	    'gas-in-tank
	    "Fill the tank and try starting the engine again.")
  (add-node 'engine-wont-turn-over
	    "Do you hear any sound when you turn the key?"
	    'sound-when-turn-key
	    'no-sound-when-turn-key)
  (add-node 'no-sound-when-turn-key
	    "Is the battery voltage low?"
	    "Replace the battery"
	    'battery-voltage-ok)
  (add-node 'battery-voltage-ok
	    "Are the battery cables dirty or loose?"
	    "Clean the cables and tighten the connections."
	    'battery-cables-good) )

(defun add-node (name question yes-case no-case)
  (setf node (make-node :name name
			:question question
			:yes-case yes-case
			:no-case no-case))
  (push node *node-list*)
  (node-name node))
; it's also possible to push directly onto the list without the separate variable

(defun find-node (name list)
  (dolist (node list)
    (when (equal (node-name node) name)
      (return node))))

(defun process-node (name)
  (if (stringp name)
      (format t "~&~s~%" name)
      (let ((node (find-node name *node-list*)))
	(if (null node)
	    (format t "~s was not found" name)
	    (if (y-or-n-p (node-question node))
		(process-node (node-yes-case node))
		(process-node (node-no-case node)))))))

