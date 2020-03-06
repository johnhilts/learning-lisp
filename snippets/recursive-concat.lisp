(defun concat-lists (seq1 seq2)
  (if (null seq1)
      seq2
      (cons (car seq1) (concat-lists (cdr seq1) seq2))))

(setq newlist (concat-lists '(1 2 3) '(4 5 6)))
;(format nil "~a" (newlist))
; should output this: (1 2 3 4 5 6)
(format nil (concat-lists '(1 2 3) '(4 5 6)))