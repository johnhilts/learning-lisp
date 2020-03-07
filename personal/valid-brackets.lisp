(setf **test-cases-valid-brackets**
  '(({ } { })
    ({ { { } } })
    ({ })
    ({ { } } })
    ({ { { } })
    (} { } {)))

(defun is-valid-brackets? ()
    "are open close setup right?"
  (mapcar
   #'(lambda (e)
       (cond
	 ((equal
	   (length(remove-if-not #'(lambda (b) (equal '{ b)) e))
	   (length(remove-if-not #'(lambda (b) (equal '} b)) e)))
	  t)
	 (t nil))
	 ) **test-cases-valid-brackets**))

