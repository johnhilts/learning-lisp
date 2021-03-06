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
  ; update - this wouldn't count as TCO because of the append
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

(defun my-every-other-item (list)
  "return a list that has every other itme from the original list"
  (cond ((null list) nil)
	(t
	 (append
	  (list (car list))
	  (my-every-other-item (cddr list))
	  ))))

(defun my-left-half (list)
  "get the left half of a list"
  (my-left-half-helper list (/ (length list) 2)))

(defun my-left-half-helper (list n)
  "helper for my-left-half"
  (cond ((>= 0 n) nil)
	(t (cons (car list) (my-left-half-helper (cdr list) (- n 1))))))

(defun my-merge-the-lists (list1 list2)
  "merge 2 sorted lists and maintain the order"
  (cond ((null list1) list2)
	((null list2) list1)
	((<= (car list1) (car list2)) (cons (car list1) (my-merge-the-lists (cdr list1) list2)))
	(t (cons (car list2) (my-merge-the-lists list1 (cdr list2))))))

#||
format: name father mother
||#
(defvar **the-family-tree**
      '((colin nil nil)
	(deirdre nil nil)
	(arthur nil nil)
	(kate nil nil)
	(frank nil nil)
	(linda nil nil)
	(suzanne colin deirdre)
	(bruce arthur kate)
	(charles arthur kate)
	(david arthur kate)
	(ellen arthur kate)
	(george frank linda)
	(hillary frank linda)
	(andre nil nil)
	(tamara bruce suzanne)
	(vincent bruce suzanne)
	(wanda nil nil)
	(ivan george ellen)
	(julie george ellen)
	(marie george ellen)
	(nigel andre hillary)
	(frederick nil tamara)
	(zelda vincent wanda)
	(joshua ivan wanda)
	(quentin nil nil)
	(robert quentin julie)
	(olivia nigel marie)
	(peter nigel marie)
	(erica nil nil)
	(yvette robert zelda)
	(diane peter erica)))

(defun father (person)
  "get person's faaahthah (luuuke)"
  (cadr (assoc person **the-family-tree**)))

(defun mother (person)
  "get person's mother"
  (caddr (assoc person **the-family-tree**)))

(defun parents (person)
  "get person's parents"
  (remove-if #'(lambda (e) (null e))
	     (cdr (assoc person **the-family-tree**))))

(defun children (person)
  "get person's children"
  (if (null person)
      nil
      (mapcar
       #'(lambda (e)
	   (car e))
       (union
	(remove-if-not
	 #'(lambda (e)
	     (equal person (cadr e)))
	 **the-family-tree**)
	(remove-if-not
	 #'(lambda (e)
	     (equal person (caddr e)))
	 **the-family-tree**)))))

(defun siblings (person)
  "get siblings - I think this has to be solved with recursion ..."
  (remove-if
   #'(lambda (e)
       (equal person e))
   (union
    (children (father person))
    (children (mother person)))))
  
(defun mapunion (function list)
  "(+ mapcar union) probably"
  (reduce #'union (mapcar function list)))

(defun grandparents (person)
  "get a person's grandparents"
  (mapunion #'parents (parents person)))

(defun cousins (person)
  "get a person's cousins"
  (mapunion #'children (mapunion #'siblings (parents person))))

(defun is-descended-from (descendent ancestor)
  "is descendent descended from ancestor?"
  (cond ((null descendent) nil)
	((member ancestor (parents descendent)) t)
	((or
	  (is-descended-from (father descendent) ancestor)
	  (is-descended-from (mother descendent) ancestor)) t)))

(defun ancestors (x)
  (cond ((null x) nil)
	(t (union
	    (parents x)
	    (union (ancestors (father x))
		   (ancestors (mother x)))))))

(defun generation-gap (descendent ancestor)
  "how many generations are the 2 separated?"
  (cond ((not (is-descended-from descendent ancestor)) nil)
	((equal descendent ancestor) 0)
	(t (reduce #'+
		   (mapcar #'(lambda (e)
			       (if (is-descended-from e ancestor)
				   (+ 1 (generation-gap e ancestor))
				   (if (equal e ancestor)
				       1
				       0)))
			   (parents descendent))))))

#||
in the answer from the book, look how it leverages or with nill and a number
compare to this expression that works the same way:
(or () 1) => 1
very elegant!
||#
(defun generation-gap-answer-from-book (x y)
  (g-gap-helper x y 0))
(defun g-gap-helper (x y n)
  (cond ((null x) nil)
	((equal x y) n)
	(t (or (g-gap-helper
		(father x) y (1+ n))
	       (g-gap-helper
		(mother x) y (1+ n))))))

(defun my-count-up-tco (n)
  "count up with proper TCO"
  (my-count-up-tco-helper n ()))

(defun my-count-up-tco-helper (n list)
  "count up tco helper"
  (cond ((zerop n) list)
	(t (my-count-up-tco-helper (- n 1) (cons n list)))))
  
(defun my-union-tco (list1 list2)
  "recursive union using tco"
  (cond ((null list1) list2)
	(t (my-union-tco (cdr list1)
	    (if (member (car list1) list2) list2 (cons (car list1) list2))))))

(defun my-intersection-tco (list1 list2)
  "intersection with proper TCO"
  (my-intersection-tco-helper list1 list2 () (length list1)))

(defun my-intersection-tco-helper (list1  list2 result n)
  "intersection tco helper"
  (cond ((zerop n) result)
	(t (my-intersection-tco-helper
	 (cdr list1)
	 list2
	 (if (member (car list1) list2)
	     (cons (car list1) result)
	     result) (- n 1)))))

(defun my-set-difference-tco (list1 list2)
  "set-difference with proper TCO"
  (my-set-difference-tco-helper list1 list2 () (length list1)))

(defun my-set-difference-tco-helper (list1  list2 result n)
  "set-difference tco helper"
  (cond ((zerop n) result)
	(t (my-set-difference-tco-helper
	 (cdr list1)
	 list2
	 (if (not (member (car list1) list2))
	     (cons (car list1) result)
	     result) (- n 1)))))

(defun my-tree-find-if (fn list)
  "apply fn to list and return first non nil atom"
  ; not using tco
  (cond ((null list) nil)
	((and
	  (atom (car list))
	  (funcall fn (car list)))
	 (car list))
	((and
	  (listp (car list))
	  (my-tree-find-if fn (car list)))
	 (my-tree-find-if fn (car list)))
	(t
	 (my-tree-find-if fn (cdr list)))))

; example from book
(defun count-up (n)
  (labels ((count-up-recursively (cnt) ; inner function name is count-up-recursively
	     (if (> cnt n) nil
		 (cons cnt
		       (count-up-recursively
			(+ cnt 1)))))) ; "body of inner function ends here
    (count-up-recursively 1))) ; ... but,there's a call made to the inner function that's still inside the labels block. Examples on clhs are the same format.
	
#||
here's an example from stack overflow; it defines 2 nested functions.
This gives me a clearer idea of how labels works...
||#
(defun thing (x)
  (labels ((helper (y) (loop for i from x to y collect i))
	   (double-it (num) (* 2 num)))
    (helper (double-it 10))))

(defun my-thing (x)
  (labels ((helper (y) (loop for i from x to y collect i))
	   (double-it (num) (* 2 num)))
    (let ((my-value (helper (double-it 10))))
      (print my-value))))

#||
An arithmetic expression is either a number, or a three-element list
whose first and third elements are arithmetic expressions and whose
middle element is one of +, -, *, or /.
||#
(defun my-arith-eval-core (equation)
  "parse normal equation and evaluate it"
  (labels ((my-get-number (possible-number)
	     (if (atom possible-number)
		 possible-number
		 (my-arith-eval-core possible-number))))
    (let ((num1 (my-get-number (car equation)))
	  (op (cadr equation))
	  (num2 (my-get-number (caddr equation))))
      (funcall op num1 num2))))

; book solution ... much more elegant!      
(defun book-arith-eval (exp)
  (cond ((numberp exp) exp)
	(t (funcall (second exp)
		    (arith-eval (first exp))
		    (arith-eval (third exp))))))

(defun legal-equation? (equation)
  "is this a legal equation?"
  (cond
    ((null equation) nil)
    ((numberp equation) t)
    ((and
      (listp equation)
      (legal-equation? (car equation))
      (member (cadr equation) '(* / + -))
      (legal-equation? (caddr equation))) t)
    (t nil)))

;from the book
(defun factors (n)
  (factors-help n 2))
(defun factors-help (n p)
  (cond ((equal n 1) nil)
	((zerop (rem n p))
	 (cons p (factors-help (/ n p) p)))
	(t (factors-help n (+ p 1)))))

(defun factor-tree (n)
  "similar to the factors function, but output the tree"
  (factor-tree-helper n 2))

(defun factor-tree-helper (n p)
  "helper for factor-tree - why not use labels??"
  (cond ((equal n 1) nil)
	((equal n p) n)
	((zerop (rem n p))
	 (list n p (factor-tree-helper (/ n p) p)))
	(t (factor-tree-helper n (+ p 1)))))
	 
