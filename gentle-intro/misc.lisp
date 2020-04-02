(prog ((accum 0) (i 1))
 again
 (when (<= i 10)
   (incf accum i)
   (incf i)
   (go again))
   (return accum))
