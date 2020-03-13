(defun my-add-up (x)
  "add up numbers recursively"
  (cond ((null x) 0)
	(t (+ (car x) (my-add-up (cdr x))))))

(defun my-alloddp (x)
  "check all elements are odd recursively"
  (cond ((null x) t)
	((and
	  (oddp (car x))
	  (my-alloddp (cdr x))))
	(t nil)))

(defun recursive-member (find list)
  "NIH member with recursion"
  (if (null list) nil
      (if (equal find (car list))
	  list
	  (recursive-member find (cdr list)))))

(defun recursive-assoc (find table)
  "NIH assoc with recursion"
  (if (null table) nil
      (if (equal find (car (car table)))
	  (car table)
	  (recursive-assoc find (cdr table)))))

(defun recursive-nth (index list)
  "NIH nth with recursion"
  (if (equal 0 index) (car list)
      (recursive-nth (- index 1) (cdr list))))

; Fib(n) = Fib(n-1) + Fib(n-2)

(defun my-fibonacci (n)
  "recursive fibonacci"
  (cond
    ((or
     (equal 0 n)
     (equal 1 n)) 1)
    (t (+ (my-fibonacci (- n 1)) (my-fibonacci (- n 2))))))

(defun anyoddp (x)
  (cond ((null x) nil)
	((oddp (first x)) t)
	(t (anyoddp (rest x)))))

(defun find-first-odd-num (list)
  "find the 1st odd number in the list"
  (cond ((null list) nil)
	((oddp (car list)) (car list))
	(t (find-first-odd-num (cdr list)))))

(defun find-last-element-in-list (list)
  "find the last element in a list"
  (cond ((null (cdr list)) (car list))
	(t (find-last-element-in-list (cdr list)))))

(defun add-nums (number)
  "add the numbers"
  (cond ((equal 0 number) 0)
	(t (+ number (add-nums (- number 1))))))

(defun all-equal? (list)
  "are all elements in a list equal?"
  (cond ((<= (length list) 1) t)
	((and
	  (equal (car list) (cadr list))
	  (all-equal? (cdr list))))
	(t nil)))

(defun do-a-count-down (number)
  "count down from a number using cons recursion"
  (cond ((equal 0 number) '(0))
	(t (cons number (do-a-count-down (- number 1))))))

(defun do-another-count-down (number)
  "count down from a number with a different kind of recursion"
  (cond ((equal 0 number) '(0))
	(t (append (list number) (do-another-count-down (- number 1))))))

(defun do-a-count-down-to-1 (number)
  "only count down to 1"
  (cond ((equal 1 number) '(1))
	(t (cons number (do-a-count-down-to-1 (- number 1))))))

(defun get-squares-for-list-with-cons (list)
  "return a list of squares based on input list using cons"
  (cond ((null list) nil)
	(t (cons (* (car list) (car list)) (get-squares-for-list (cdr list))))))

(defun get-squares-for-list (list)
  "return a list of squares based on input list using cons"
  (cond ((null list) nil)
	(t (append (list (* (car list) (car list))) (get-squares-for-list (cdr list))))))

(defun recursive-compare-list-lengths (list1 list2)
  "compare the length of 2 lists recursively"
  (cond ((and (null list1) (null list2)) 'same-length)
	((null list1) 'second-is-longer)
	((null list2) 'first-is-longer)
	(t (recursive-compare-list-lengths (cdr list1) (cdr list2)))))

(defun sum-numeric-elements-only (list)
  "sum up any numeric elements in a list using recursion"
  (cond ((null list) 0)
	((not (numberp (car list))) (sum-numeric-elements-only (cdr list)))
	(t (+ (car list) (sum-numeric-elements-only (cdr list))))))

(defun my-recursive-remove (item list)
  "recursively remove an item from a list"
  (cond ((null list) nil)
	((equal item (car list)) (my-recursive-remove item (cdr list)))
	(t (append (list (car list)) (my-recursive-remove item (cdr list))))))

(defun my-recursive-intersection (list1 list2)
  "implement intersection with recursion"
  (cond ((null list1) nil)
	((member (car list1) list2) (append (list (car list1)) (my-recursive-intersection (cdr list1) list2)))
	(t (my-recursive-intersection (cdr list1) list2))))

(defun my-recursive-intersection-for-tco (list1 list2)
  "trying an ML style version more suited for TCO"
  (remove-if #'null
	     (append
	      (if (member (car list1) list2) (list (car list1)) (list nil))
	      (cond ((null list1) (list nil))
		    (t (my-recursive-intersection-for-tco (cdr list1) list2))))))

(defun my-recursive-set-difference (list1 list2)
  "implement set-difference with recursion"
  (cond ((null list1) nil)
	((not (member (car list1) list2)) (append (list (car list1)) (my-recursive-set-difference (cdr list1) list2)))
	(t (my-recursive-set-difference (cdr list1) list2))))

(defun my-count-odds-1 (list)
  "count odds using a form of recursion"
  (+
   (cond ((null list) 0)
	 ((oddp (car list)) (+ 1 (my-count-odds-1 (cdr list))))
	 (t (my-count-odds-1 (cdr list))))))

(defun my-count-atoms (list)
  "count the atoms recursively"
  (cond ((atom list) 1)
	(t (+ (my-count-atoms (car list)) (my-count-atoms (cdr list))))))

(defun count-cons (tree)
  "according to this, anything that's not an atom is a cons cell(??)"
  (cond ((atom tree) 0)
	(t (+ 1
	      (count-cons (car tree))
	      (count-cons (cdr tree))))))

(defun my-count-cons (tree)
  "what about a !list?"
  #||
as long as we also check for an empty list
because (listp nil) is true, because it's really (listp ()) ie "empty list"
so that also means, according to this, that () is NOT a cons cell - true??
||#
  (cond ((or (null tree) (not (listp tree))) 0)
	 (t (+ 1
	     (my-count-cons (car tree))
	     (my-count-cons (cdr tree))))))

(defun my-sum-tree (tree)
  "sum all the numbers that appear in a tree"
  (cond ((atom tree) (if (numberp tree) tree 0))
	(t (+ (my-sum-tree (car tree)) (my-sum-tree (cdr tree))))))

(defun my-subst (new old list)
  "recursive version of subst - do we need to handle nested?? - why, yes!"
  (cond ((null list) nil)
	((equal (car list) old) (cons new (my-subst new old (cdr list))))
	(t
	 (cons
	  (if (listp (car list)) (my-subst new old (car list)) (car list))
	  (my-subst new old (cdr list))))))

(defun my-flatten-nested (list)
  "flatten list recursively"
  (cond ((null list) nil)
	((atom list) (list list))
	(t
	 (append
	  (my-flatten-nested (car list))
	  (my-flatten-nested (cdr list))))))

(defun get-the-paren-depth (list)
  "how deeply is a list nested?"
  (+ 
   (cond ((null list) 1)
	 ((listp (car list)) (+ 1 (get-the-paren-depth (car list))))
	 (t (get-the-paren-depth (cdr list))))))

; count-up version using helper function
(defun their-count-up (n)
  (their-count-up-recursively 1 n))

(defun their-count-up-recursively (cnt n)
  (cond ((> cnt n) nil)
	(t (cons cnt
		 (their-count-up-recursively
		  (+ cnt 1) n)))))

(defun my-count-up (n)
  "add elements to the end of the list instead of beginning - no need for helper"
   (cond ((zerop n) nil)
	 (t (append (my-count-up (- n 1)) (list n)))))

(defun make-loaf (n)
  "usage: (make-loaf 4) => '(x x x x)"
  (if (zerop n) nil (append '(x) (make-loaf (- n 1)))))

(defun bury-under-parentheses (x n)
  "bury x under n number of parentheses"
  (if (zerop n) x (list (bury-under-parentheses x (- n 1)))))

(defun make-some-pairings (list1 list2)
  "assume lists are the same size; '(1 2 3) '(a b c) => '((a 1)(b 2)(c 3))"
   (cond ((null list1) nil)
	 (t (append (list (list (car list1) (car list2))) (make-some-pairings (cdr list1) (cdr list2))))))

(defun make-some-sublists (list)
  "'(one two three) => '((one two three)(two three)(three))"
  (cond ((null list) nil)
	(t (append (list list) (make-some-sublists (cdr list))))))

(defun my-reverse (list)
  "recursive reverse"
  (my-reverse-helper list (length list)))

(defun my-reverse-helper (list n)
  "helper function"
   (cond
     ((zerop n) nil)
     (t
      (append
       (list (nth (- n 1) list))
       (my-reverse-helper list (- n 1))
       ))))

(defun my-reverse-no-helper (list)
  "no helper function"
  (cond ((null list) nil)
	(t
	 (append
	  (my-reverse-no-helper (cdr list))
	  (list (car list))
	  ))))

(defun my-union (list1 list2)
  "recursive union"
  (cond ((null list1) list2)
	((member (car list1) list2) (my-union (cdr list1) list2))
	(t (cons (car list1) (my-union (cdr list1) list2)))))

(defun the-largest-even (list)
  "assumes no negatives"
  (cond ((null list) 0)
	((oddp (car list)) (the-largest-even (cdr list)))
	(t (max (car list) (the-largest-even (cdr list))))))

(defun my-huge-exponentizer (n)
  "given n, return n**n"
  (my-huge-helper n n))

(defun my-huge-helper (n i)
  "helper for my-huge-exponentizer"
  (cond ((zerop i) 1)
	(t (* n (my-huge-helper n (- i 1))))))
