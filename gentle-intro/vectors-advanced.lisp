(coerce "Cockatoo" 'list)
; (#\C #\o #\c #\k #\a #\t #\o #\o)
(coerce '(#\b #\i #\r #\d) 'string)
; "bird"
(coerce '(foo bar baz) 'vector)
; #(FOO BAR BAZ)
(make-array 3 :element-type 'string-char
	    :initial-contents '(#\M #\o #\m))
; #(#\M #\o #\m)
(map 'list ; result type
     #'+ ; function
     '(1 2 3 4) ; sequence (list)
     '#(10 20 30 40)) ; sequence (vector) - you can have multiple!
;(11 22 33 44)

(map 'list ; result type
     #'list ; function 
     '(a b c) ; sequence (list)
     '#(1 2 3) ; sequence (array/vector)
     "xyz") ; sequence (string)
;((A 1 #\x) (B 2 #\y) (C 3 #\z))

(map nil ; no result type - only get IO side effect
     #'print ; function
     "a b") ; sequence
#||
#\a 
#\  
#\b 
NIL
||#
