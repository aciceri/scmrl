(module Dungeon (createDungeon
		 getFloorRandomCoord)
    
  (import chicken scheme)
  (use srfi-1)
  (use srfi-25) ;for arrays
  (use random-bsd)
  
  
  (define WALL-CHAR #\#)
  (define FLOOR-CHAR #\.)
  
  
  (define (createDungeon width height nRooms nCuts prob)
    (let ((grid (make-array (shape 0 width 0 height) WALL-CHAR)) (rooms '()))
      (begin
	(set! rooms (placeRandomRooms! grid nRooms width height)) ;place some random rooms
	(genMaze! grid `(,(getWallRandomCoord grid width height)) width height) ;generate a maze in the space between the rooms
	(connectRooms! grid rooms width height) ;create a single passage for every room
	(cutMaze! grid nCuts width height) ;erase dead ends of the maze
	(liteGrid! grid prob width height)) ;create some extra passages given a probability
      grid))

  
  (define (randomBetween a b)
    (+ (random (abs (- b a))) a))

  
  (define (map-matrix m x1 y1 x2 y2 f) ;apply f for every (x, y) in the rectangle (x1, x2, y1, y2)
    (for-each (lambda (y)
		(for-each (lambda (x)
			    (f x y))
			  (iota (abs (- x1 x2)) (min x1 x2) 1)))
	      (iota (abs (- y1 y2)) (min y1 y2) 1)))

  
  (define (getWallRandomCoord m width height) ;return random coordinates a cell without floor
    (let ((x (randomBetween 3 (- width 3))) (y (randomBetween 3 (- height 3))))
      (if (and (eq? (array-ref m x y) WALL-CHAR) (= (countNeighbours m x y) 0))
	  (list x y)
	  (getWallRandomCoord m width height))))

  
  (define (getFloorRandomCoord m width height)
    (let ((x (randomBetween 3 (- width 3))) (y (randomBetween 3 (- height 3))))
      (if (eq? (array-ref m x y) #\.)
	  `(,x ,y)
	  (getFloorRandomCoord m width height))))

  
  (define (digRoom! m x-start y-start width height)
    (map-matrix m x-start y-start
		(+ x-start width) (+ y-start height)
		(lambda (x y)
		  (array-set! m x y FLOOR-CHAR))))
  

  (define (spaceForRoom? m x-start y-start width height grid-width grid-height) ;check if there is space for the room in the dungeon
    (let ((is-ok #t))
      (map-matrix m (- x-start 2) (- y-start 2)
		  (+ x-start width 2) (+ y-start height 2)
		  (lambda (x y)
		    (if (or (>= x grid-width)
			    (= x 0)
			    (>= y grid-height)
			    (= y 0))
			(set! is-ok #f)
			(when (eq? (array-ref m x y) FLOOR-CHAR)
			  (set! is-ok #f)))))
      is-ok))
  
  
  (define (randomRoom m grid-width grid-height) ;return a random room
    (let ((x (randomBetween 4 (- grid-width 12)))
	  (y  (randomBetween 4 (- grid-height 12)))
	  (width  (randomBetween 4 8))
	  (height  (randomBetween 4 8)))
      (if (spaceForRoom? m x y width height grid-width grid-height)
	  (list x y width height)
	  (randomRoom m grid-width grid-height))))
  

  (define (placeRandomRooms! m n grid-width grid-height) ;place n random rooms (not overlapping) in the dungeon
    (fold (lambda (i acc)
	    (let ((room (randomRoom m grid-width grid-height)))
	      (begin
		(apply digRoom! (cons m room))
		(cons room acc))))
	  '()
	  (iota n)))
  
  
  (define (connectRooms! m rooms width height) ;connects every room to the maze
    (for-each (lambda (r)
		(connectRoom! m r 10 width height))
	      rooms))
  

  (define (connectRoom! m room attempt width height) ;create a floor cell connecting the room, may fail (nothing happens)
    (let ((room-x (first room)) (room-y (second room))
	  (room-width (third room)) (room-height (fourth room))
	  (dir 3) (door-x 0) (door-y 0))
      (begin
	(cond ((= dir (random 4))
	       (begin
		 (set! door-y (randomBetween room-y (+ room-y room-height)))
		 (set! door-x (+ room-x room-width))))
	      ((= dir 1)
	       (begin
		 (set! door-y (randomBetween room-y (+ room-y room-height)))
		 (set! door-x (- room-x 1))))
	      ((= dir 2)
	       (begin
		 (set! door-y (- room-y 1))
		 (set! door-x (randomBetween room-x (+ room-x room-width)))))
	      ((= dir 3)
	       (begin
		 (set! door-y (+ room-y room-height))
		 (set! door-x (randomBetween room-x (+ room-x room-width))))))
	(if (= (countNeighbours m door-x door-y) 2)
	    (array-set! m door-x door-y FLOOR-CHAR)
	    (when (> attempt 0)
	      (connectRoom! m room (- attempt 1) width height))))))
  
  
  (define (genMaze! m stack width height) ;generate a maze using a recursive backtracker method
    (if (eq? stack '())
	'() ;if the maze is terminated
	(begin
	  (array-set! m (first (car stack)) (second (car stack)) FLOOR-CHAR)
	  (let ((new-coord (getNewCoord m (car stack) width height)))
	    (if (eq? new-coord '())
		(genMaze! m (cdr stack) width height)
		(genMaze! m (cons new-coord stack) width height))))))

  
  (define (getNewCoord m coord width height) ;get a new coordinate for the maze digger
    (let ((x (first coord)) (y (second coord)))
      (if (or (<= x 1) (<= y 1) (>= x (- width 2)) (>= y (- height 2)))
	  '()
	  (let ((directions '()))
	    (begin
	      (when (placeable? m x y 'e)
		(set! directions (cons 'e directions)))
	      (when (placeable? m x y 'w)
		(set! directions (cons 'w directions)))
	      (when (placeable? m x y 's)
		(set! directions (cons 's directions)))
	      (when (placeable? m x y 'n)
		(set! directions (cons 'n directions)))
	      (if (eq? directions '())
		  '()
		  (let ((dir (list-ref directions (random (length directions)))))
		    (cond ((eq? dir 'e) (list (+ x 1) y))  
			  ((eq? dir 'w) (list (- x 1) y))
			  ((eq? dir 's) (list x (+ y 1)))
			  ((eq? dir 'n) (list x (- y 1)))))))))))
  
  
  (define (placeable? m x y dir) ;check if a cell can be placed to generate a maze's corridor
    (let ((is-ok #t))
      (cond ((eq? dir 'e)
	     (when (or (eq? (array-ref m (+ x 1) y) FLOOR-CHAR)
		       (eq? (array-ref m (+ x 1) (+ y 1)) FLOOR-CHAR)
		       (eq? (array-ref m (+ x 1) (- y 1)) FLOOR-CHAR)
		       (eq? (array-ref m (+ x 2) y) FLOOR-CHAR)
		       (eq? (array-ref m (+ x 2) (+ y 1)) FLOOR-CHAR)
		       (eq? (array-ref m (+ x 2) (- y 1)) FLOOR-CHAR))
	       (set! is-ok #f)))
	    
	    ((eq? dir 'w)      
	     (when (or (eq? (array-ref m (- x 1) y) FLOOR-CHAR)
		       (eq? (array-ref m (- x 1) (+ y 1)) FLOOR-CHAR)
		       (eq? (array-ref m (- x 1) (- y 1)) FLOOR-CHAR)
		       (eq? (array-ref m (- x 2) y) FLOOR-CHAR)
		       (eq? (array-ref m (- x 2) (+ y 1)) FLOOR-CHAR)
		       (eq? (array-ref m (- x 2) (- y 1)) FLOOR-CHAR))
	       
	       (set! is-ok #f)))
	    
	    ((eq? dir 's)
	     (when (or (eq? (array-ref m x (+ y 1)) FLOOR-CHAR)
		       (eq? (array-ref m (+ x 1) (+ y 1)) FLOOR-CHAR)
		       (eq? (array-ref m (- x 1) (+ y 1)) FLOOR-CHAR)
		       (eq? (array-ref m x (+ y 2)) FLOOR-CHAR)
		       (eq? (array-ref m (+ x 1) (+ y 2)) FLOOR-CHAR)
		       (eq? (array-ref m (- x 1) (+ y 2)) FLOOR-CHAR))
	       (set! is-ok #f)))
	    
	    ((eq? dir 'n)
	     (when (or (eq? (array-ref m x (- y 1)) FLOOR-CHAR)
		       (eq? (array-ref m (+ x 1) (- y 1)) FLOOR-CHAR)
		       (eq? (array-ref m (- x 1) (- y 1)) FLOOR-CHAR)
		       (eq? (array-ref m x (- y 2)) FLOOR-CHAR)
		       (eq? (array-ref m (+ x 1) (- y 2)) FLOOR-CHAR)
		       (eq? (array-ref m (- x 1) (- y 2)) FLOOR-CHAR))
	       (set! is-ok #f))))
      
      is-ok))
  

  (define (countNeighbours m x y) ;count neighbour cells (only nord/sud/east/west)
    (let ((n 0))
      (begin
	(when (eq? (array-ref m (+ x 1) y) FLOOR-CHAR)
	  (set! n (+ n 1)))
	(when (eq? (array-ref m (- x 1) y) FLOOR-CHAR)
	  (set! n (+ n 1)))
	(when (eq? (array-ref m x (+ y 1)) FLOOR-CHAR)
	  (set! n (+ n 1)))
	(when (eq? (array-ref m x (- y 1)) FLOOR-CHAR)
	  (set! n (+ n 1)))
	n)))
  

  (define (cutMaze! m n width height) ;cut n dead ends of maze
    (for-each (lambda (i)
		(map-matrix m 0 0 width height 
			    (lambda (x y)
			      (when (and
				     (eq? (array-ref m x y) FLOOR-CHAR)
				     (= (countNeighbours m x y) 1))
				
				(array-set! m x y WALL-CHAR)
				))))
	      (iota n)))
  
  
  (define (liteGrid! m probability width height) ;erase certain cells to create new passages
    (map-matrix m 1 1 (- width 1) (- height 1)
		(lambda (x y)
		  (when (< (random 100) probability)
		    (begin
		      (when (and (eq? (array-ref m (+ x 1) y) FLOOR-CHAR)
				 (eq? (array-ref m x y) WALL-CHAR)
				 (eq? (array-ref m (- x 1) y) FLOOR-CHAR))
			(array-set! m x y FLOOR-CHAR))
		      (when (and (eq? (array-ref m x (+ y 1)) FLOOR-CHAR)
				 (eq? (array-ref m x y) WALL-CHAR)
				 (eq? (array-ref m x (- y 1)) FLOOR-CHAR))
			(array-set! m x y FLOOR-CHAR)))))))
  )

