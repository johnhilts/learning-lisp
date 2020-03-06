(defvar **rooms** 
      '(
	(living-room 
	 (north front-stairs)
	 (south dining-room)
	 (east kitchen)
	 )
	(upstairs-bedroom   
	 (west library)
	 (south front-stairs)
	 )
	(dining-room        
	 (north living-room)
	 (east pantry)
	 (west downstairs-bedroom)
	 )
	(kitchen            
	 (west living-room)
	 (south pantry)
	 )
	(pantry             
	 (north kitchen)
	 (west dining-room)
	 )
	(downstairs-bedroom 
	 (north back-stairs)
	 (east dining-room)
	 )
	(back-stairs        
	 (south downstairs-bedroom)
	 (north library)
	 )
	(front-stairs       
	 (north upstairs-bedroom)
	 (south living-room)
	 )
	(library            
	 (east upstairs-bedroom)
	 (south back-stairs)
	 )
	)
)

(defvar **loc** 'library)

(defun choices (room)
"return options by room name"
(cdr (assoc room **rooms**)))

(defun look (direction room)
  (second (assoc direction (choices room))))

(defun set-robbie-location (place)
  "Moves Robbie to PLACE by settingthe variable LOC."
  (setf **loc** place))

(defun how-many-choices ()
  "how many choices in current room?"
  (length (cdr (assoc **loc** **rooms**))))

(defun upstairsp (room)
  "is room upstairs?"
  (cond ((or 
	  (equal room 'library) 
	  (equal room 'upstairs-bedroom)) t)
	(t nil)))

(defun onstairsp (room)
  "is room on the stairs?"
  (cond ((or 
	  (equal room 'front-stairs) 
	  (equal room 'back-stairs)) t)
	(t nil)))

(defun wheres-robbie? ()
  "where's robbie at?"
  (append (list 'robbie 'is)
	(cond ((upstairsp **loc**) (list 'upstairs 'in) )
	      ((onstairsp **loc**) (list 'on) )
	      (t (list 'downstairs 'in) ))
	(list 'the **loc**)))

(defun move-robbie (direction)
  "move robbie to a different room by direction"
  (let ((new-room (look direction **loc**)))
    (if 
     (and 
      new-room 
      (set-robbie-location new-room)
      )
     (wheres-robbie?)
     '(ouch! robbie hit a wall!))))

	  
