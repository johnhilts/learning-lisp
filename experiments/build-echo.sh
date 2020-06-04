sbcl --noinform --load  "/home/jfh/code/lisp/source/learning-lisp/experiments/echo-cmd.lisp" --eval "(sb-ext:save-lisp-and-die \"echo.exe\" :executable t :toplevel 'echo)"

