(use Terminal)
(use Dungeon)
(use Fov)
(use Dijkstra)
(use srfi-25) ;to delete



(begin
  (createScreen!)

  (define w 70)
  (define h 40)
  (define d (createDungeon w h 14 30 10))
  (define explored (make-array (shape 0 w 0 h) #\space))

  (define xPlayer)
  (define yPlayer)
  (let ((coord (getFloorRandomCoord d w h)))
    (set! xPlayer (first coord))
    (set! yPlayer (second coord)))

  (define xEnd)
  (define yEnd)
  (let ((coord (getFloorRandomCoord d w h)))
    (set! xEnd (first coord))
    (set! yEnd (second coord)))

  (define dm)
 
  (pushMsg! "Welcome adventurer!")
  (pushMsg! (format #f "Target: ~a, ~a" xEnd yEnd))
  
  (let loop ((running #t))
    (when running

      (let ((fov (calculateFov d w h xPlayer yPlayer 5)))
	(for-each (lambda (x)
		    (for-each (lambda (y)
				(when (array-ref fov x y)
				  (array-set! explored x y (array-ref d x y))))
			      (iota h)))
		  (iota w))
	(drawMap! explored fov xPlayer yPlayer w h))

      
      (drawPlayer!)

      ;(set! dm (createDijkstraMap d w h xPlayer yPlayer))
      ;(let ((path (calculatePath dm w h xEnd yEnd)))
	;(drawPath! d path xPlayer yPlayer))
      
      (pushMsg! (format #f "Position (~a, ~a)" xPlayer yPlayer))

      (let ((key (getKey)))
	(cond ((eq? key #\w)
	       (when (eq? (array-ref d xPlayer (- yPlayer 1)) #\.) (set! yPlayer (- yPlayer 1))))
	      ((eq? key #\a)
	       (when (eq? (array-ref d (- xPlayer 1) yPlayer) #\.) (set! xPlayer (- xPlayer 1))))
	      ((eq? key #\s)
	       (when (eq? (array-ref d xPlayer (+ yPlayer 1)) #\.) (set! yPlayer (+ yPlayer 1))))
          ((eq? key #\d)
	       (when (eq? (array-ref d (+ xPlayer 1) yPlayer) #\.) (set! xPlayer (+ xPlayer 1))))
	      ((eq? key #\q)
           (when (equal? (askMsg "Type 'quit' to quit: ") "quit")
             (set! running #f)))))
      (loop running)))
	     

  
  (destroyScreen! "Thanks for playing")
 


)
