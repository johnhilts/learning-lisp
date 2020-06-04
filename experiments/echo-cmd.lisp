(defun echo ()
  (let ((cal sb-ext:*posix-argv*))
    (format t "~&Command line arguments:~%~{~&~s~}" cal)
    (format t "~%~%cadr sb-ext:*posix-argv: ~s~&home-dir: ~s~%"
            (cadr sb-ext:*posix-argv*)
            (directory-namestring (user-homedir-pathname)))))
