(defvar crypto-text nil)
(defvar *encipher-table* nil)
(defvar *decipher-table* nil)


(defun init-crypto-text ()
  (setf crypto-text '("zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf" "enlpo pib slafml pvv bfwkj")))

(defun init-crypto-hash-tables ()
  (setf *encipher-table* (make-hash-table))
  (setf *decipher-table* (make-hash-table)))

(defun make-substitution ()
  )
