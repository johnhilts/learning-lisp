(defvar **my-global** '(a b c 1 2 3))

(defun my-global-op (index)
"test to see if a global variable works"
(nth index **my-global**))
