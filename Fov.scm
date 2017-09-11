(module Fov
    (calculateFov)

  (import chicken scheme)
  (use srfi-1)
  (use srfi-25)
  (use numbers)


  (define (getRing x y radius)
    (let* ((boxes '()) (theta (- (/ 1 (* 8 radius)))) (thetaInc (/ 1 (* 4 radius))))
      (begin
	(when (= (modulo radius 2) 1)
	  (set! theta (- (/ thetaInc 2))))
	
	(for-each (lambda (i)
		    (set! boxes (cons `(,(+ i x) ,(+ (- radius i) y) ,theta ,(+ theta thetaInc)) boxes))
		    (set! theta (+ theta thetaInc)))
		  (iota radius radius -1))
	(for-each (lambda (i)
		    (set! boxes (cons `(,(+ (- i) x) ,(+ (- radius i) y) ,theta ,(+ theta thetaInc)) boxes))
		    (set! theta (+ theta thetaInc)))
		  (iota radius 0 1))
	(for-each (lambda (i)
		    (set! boxes (cons `(,(+ (- i) x) ,(+ (- (- radius  i)) y) ,theta ,(+ theta thetaInc)) boxes))
		    (set! theta (+ theta thetaInc)))
		  (iota radius radius -1))
	(for-each (lambda (i)
		    (set! boxes (cons `(,(+ i x) ,(+ (- (- radius i)) y) ,theta ,(+ theta thetaInc)) boxes))
		    (set! theta (+ theta thetaInc)))
		  (iota radius 0 1))
	boxes)))
  
  
  (define (addShadow shadowQueue angleStart angleEnd)
    (if (< angleStart 0)
	(mergeShadow (mergeShadow shadowQueue (+ 1 angleStart) 1) 0 angleEnd)
	(if (null? shadowQueue)
	    `((,angleStart ,angleEnd))
	    (mergeShadow shadowQueue angleStart angleEnd))))
  
  (define (mergeShadow shadowQueue angleStart angleEnd)
    (if (insideShadow? shadowQueue angleStart angleEnd)
	shadowQueue
	(if (intersectShadow? shadowQueue angleStart angleEnd)
	    (let ((intersections (intersectionsShadow shadowQueue angleStart angleEnd)))
	      (fold (lambda (shadow l)
		      (if (memq shadow intersections)
			  (let ((thetaStart (first shadow)) (thetaEnd (second shadow)))
			    (if (and (< angleStart thetaStart) (> angleEnd thetaEnd))
				(cons `(,(min thetaStart thetaEnd angleStart angleEnd)
					,(max thetaStart thetaEnd angleStart angleEnd)) l)
				(cons `(,(min angleStart thetaStart) ,(max angleEnd thetaEnd)) l)))
			  
			  
			  (cons shadow l)))
		    
		    '() shadowQueue))
	    
	    (cons `(,angleStart ,angleEnd) shadowQueue))))
  
  
  (define (insideShadow? shadowQueue angleStart angleEnd)
    (if (< angleStart 0)
	(and (insideShadow? shadowQueue 0 angleEnd) (insideShadow? shadowQueue (+ angleStart 1) 1))
	(not (null? (filter (lambda (shadow)
			      (let ((thetaStart (first shadow)) (thetaEnd (second shadow)))
				(and (>= angleStart thetaStart) (<= angleEnd thetaEnd))))
			    shadowQueue)))))
  
  (define (intersectionsShadow shadowQueue angleStart angleEnd)
    (filter (lambda (shadow)
	      (let ((thetaStart (first shadow)) (thetaEnd (second shadow)))
		(or (and (< angleStart thetaStart) (> angleEnd thetaStart))
		    (and (> angleEnd thetaEnd) (< angleStart thetaEnd)))))
	    shadowQueue))
  
  
  (define (intersectShadow? shadowQueue angleStart angleEnd)
    (not (null? (intersectionsShadow shadowQueue angleStart angleEnd))))
  
  
  (define (calculateFov grid width height xStart yStart radius)
    (let ((shadowQueue '()) (fovGrid (make-array (shape 0 width 0 height) #f)))
      (for-each (lambda (rho)
		  (for-each (lambda (cell)
			      (let ((x (first cell)) (y (second cell))
				    (thetaStart (third cell)) (thetaEnd (fourth cell)))
				(when (not (insideShadow? shadowQueue thetaStart thetaEnd))
				  (begin
				    (array-set! fovGrid x y #t)
				    (when (eq? (array-ref grid x y) #\#)
				      (set! shadowQueue (addShadow shadowQueue thetaStart thetaEnd)))))))
			    (getRing xStart yStart rho)))
		
		(iota radius 1 1))
      fovGrid))

  )
