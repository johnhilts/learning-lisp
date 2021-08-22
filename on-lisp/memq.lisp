(defmacro memq (obj lst)
  `(member ,obj ,lst :test #'eq))
