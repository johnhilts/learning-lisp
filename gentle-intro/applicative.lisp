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
