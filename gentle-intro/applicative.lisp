(defun add-one (x)
  "add one to a number"
  (+ 1 x))

; (mapcar #'add-one '(1 3 5 7 9))

(defvar **daily-planet** 
  '((olsen jimmy 123-76-4535 cub-reporter)
   (kent  clark 089-52-6787 reporter)
   (lane  lois  951-26-1438 reporter)
   (white perry 355-16-7439 editor)))

; (mapcar #'caddr **daily-planet**)

(defun >5? (x)
  "greater than 5?"
  (> x 5))

; (mapcar #'>5? '(2 0 3 4 5 6 7))

(defun my-lambda (x)
  "test a lambda expression"
  (funcall #'(lambda (x) (- x 7)) x))

(defun my-other-lambda (x)
  "returns t for t or nil"
  (funcall #'(lambda (x) 
	       (or
		(equal t x)
		(equal nil x)
		)
	       ) x))

(defun flip-elements (x)
  "change A B A B to B A B A"
  (mapcar #'(lambda (i) (if (equal i (car x)) (cadr x) (car x))) x))

(defun roughly-equal? (x k)
  "get first element in x roughly equal to k"
  (find-if #'(lambda (e) 
	       (and
		(>= e (- k 10))
		(<= e (+ k 10))
		)
	       ) x))

(defun find-nested (x)
  "find first nested list"
  (find-if #'(lambda (e)
	       (and
		(listp e)
		e ; this will eval to t
		)
	       ) x))

(defvar **note-table**
  '((C 1)
    (F-SHARP 7)
    (C-SHARP 2)
    (G 8)
    (D 3)
    (G-SHARP 9)
    (D-SHARP 4)
    (A 10)
    (E 5)
    (A-SHARP 11)
    (F 6)
    (B 12)))

; (mapcar #'(lambda (x) (write x)(terpri)) **note-table**)

(defun find-number-by-note (e)
  "is this required?"
  (cadr (assoc e **note-table**)))

(defun note-numbers (x)
  "get numbers by note"
  (mapcar #'find-number-by-note x))

(defun my-reverse-lookup (e)
  "I can't believe this stupid comment part is actually required!"
  (car (find-if #'(lambda (y) 
		    (equal e (cadr y))) **note-table**)))

(defun number-notes (x)
  "get note by number"
  (mapcar #'my-reverse-lookup x))

(defun raise-notes (n x)
  "raise notes by n half steps"
  (mapcar #'(lambda (e) (+ n e)) x))

(defun normalize-notes (x)
  "normalize a list of notes"
  (mapcar #'(lambda (e)
	      (cond ((> e 12) (- e 12))
		    ((< e 1) (+ e 12))
		    (t e))) x))

#||
(note-numbers '(E D C D E E E))
(number-notes '(5  3  1  3  5 5 5))
(RAISE-NOTES 5 '(5 3 1 3 5 5 5))
(NORMALIZE-NOTES '(6 10 13))
||#

(defun transpose (n song)
  "transpose a song by n half steps"
  (number-notes (normalize-notes (raise-notes n (note-numbers song)))))

(defun between-1-and-5? (x)
  "pick out the numbers in a list that are > 1  and <5"
  (remove-if-not #'(lambda (e) (and (> e 1) (< e 5))) x))

(defun how-many-thes? (x)
  "count number of thes in a list"
  (length (remove-if-not #'(lambda (e) (equal 'the e)) x)))

(defun show-lists-with-2-elements (x)
  "extract lists with 2 elements from a list of lists"
  (remove-if-not #'(lambda (e) (equal 2 (length e))) x))

(defun my-rank (x)
  "get card rank"
  (car x))

(defun my-suit (x)
  "get card suit"
  (cadr x))

(defvar **my-hand**
  '((3 hearts)
    (5 clubs)
    (2 diamonds)
    (4 diamonds)
    (ace spades)))

(defun my-count-suit (suit hand)
  "how many of a suit are in a hand of cards?"
  (length (remove-if-not #'(lambda (e) (equal suit (my-suit e))) hand)))

(defvar **my-colors**
  '((clubs black)
    (diamonds red)
    (hearts red)
    (spades black)))

(defun color-of-card (card)
  "get the color of a card"
  (cadr (find-if #'(lambda (e) (equal (my-suit card) (car e))) **my-colors**)))

(defun first-red-card (hand)
  "get first red card of a hand"
  (find-if #'(lambda (e) (equal 'red (color-of-card e))) hand))

(defun the-black-cards (hand)
  "get all the black cards"
  (remove-if-not #'(lambda (e) (equal 'black (color-of-card e))) hand))

(defun what-ranks? (suit hand)
  "what ranks are there for the matching suits in a hand?"
  (mapcar #'my-rank (remove-if-not #'(lambda (e) (equal suit (my-suit e))) hand)))

(defvar **all-ranks** '(2 3 4 5 6 7 8 9 10 jack queen king ace))

(defun beforep (x y l)
  "does x appear before y in l?"
  (member y (member x l)))

(defun higher-card-rank? (card1 card2)
  "1st card higher rank than 2nd card?"
  (if (beforep (my-rank card1) (my-rank card2) **all-ranks**) t nil))

(defun show-high-card (hand)
  "what's the highest ranked card in a hand?"
  (car (remove-if-not #'(lambda (l) l) (mapcar #'(lambda (rank) (find-if #'(lambda (card) (equal rank (my-rank card))) hand)) (reverse **all-ranks**)))))

(defun all-odd? (l)
  "are all the numbers in a list odd?"
  (every #'oddp l))

(defun none-odd? (l)
  "are none of the numbers odd?"
  (every #'(lambda (e) (not (oddp e))) l))

(defun not-all-odd? (l)
  "return t if at least one !odd"
  (not (every #'oddp l)))

(defun not-none-odd? (l)
  "return t if at least one odd"
  #||
  this doesn't work because (every nil) returns t
  (every #'oddp (remove-if-not #'oddp l)))
  ||#
  (not (equal nil (find-if #'oddp l))))

(defvar **block-database**
      '((b1 shape brick)
	(b1 color green)
	(b1 size small)
	(b1 supported-by b2)
	(b1 supported-by b3)
	(b2 shape brick)
	(b2 color red)
	(b2 size small)
	(b2 supports b1)
	(b2 left-of b3)
	(b3 shape brick)
	(b3 color red)
	(b3 size small)
	(b3 supports b1)
	(b3 right-of b2)
	(b4 shape pyramid)
	(b4 color blue)
	(b4 size large)
	(b4 supported-by b5)
	(b5 shape cube)
	(b5 color green)
	(b5 size large)
	(b5 supports b4)
	(b6 shape brick)
	(b6 color purple)
	(b6 size large)))

(defun match-element? (e1 e2)
  "e1 == e2 || e2 == '?'"
  (or
   (equal e1 '?)
   (equal e2 '?)
   (equal e1 e2)))

(defun match-triple? (assertion pattern)
  "does assertion match the pattern?"
  (and
   (match-element? (car assertion) (car pattern))
   (match-element? (cadr assertion) (cadr pattern))
   (match-element? (caddr assertion) (caddr pattern))))

#||
(FETCH '(B2 COLOR ?)) should  return  ((B2  COLOR  RED)),  
(FETCH '(? SUPPORTS B1)) should return ((B2 SUPPORTS B1) (B3 SUPPORTS B1))
||#

(defun fetch (pattern)
  "return all entries from the database that match the pattern"
  (remove-ibf-not #'(lambda (e) (match-triple? e pattern)) **block-database**))

(defun get-color-pattern-for-block (block)
  "get color pattern for a block"
  (cons block '(color ?)))

(defun get-block-supporters (block)
  "make a list of supporters for a block"
  (mapcar #'(lambda (e) (car e)) (fetch (list '? 'supports block))))

(defun supported=by-cubes? (block)
  "is a block supported by cubes?"
  (every #'(lambda (e) e) (mapcar #'(lambda (e) (fetch (list e 'shape 'cube))) (get-block-supporters block))))

(defun block-description1 (block)
  "get all assertions matching a block"
  (fetch (list block '? '?)))

(defun block-description2 (block)
  "get all assertions matching a block, with block name stripped"
  (mapcar #'(lambda (e) (cdr e)) (block-description1 block)))

(defun block-description (block)
  "get block description"
  (reduce #'(lambda (acc e) (append acc e)) (block-description2 block)))

(defun anyoddp? (x)
  "any odds?"
  (if (null x) nil
      (if (oddp (car x)) t
	  (anyoddp? (cdr x)))))
