(module Dijkstra
    (createDijkstraMap
     calculatePath)

  
  (use srfi-1)
  (use srfi-25)
  (use extras)


  (define (createDijkstraMap dungeon width height xStart yStart)
    (let ((dijkstraMap (make-array (shape 0 width 0 height) +inf.0)))
      (begin
        (generateDijkstraMap! dungeon dijkstraMap xStart yStart width height)
        dijkstraMap)))


  (define (generateDijkstraMap! dungeon dijkstraMap xStart yStart width height)
    (array-set! dijkstraMap xStart yStart 1)
    (let loop ((stack `((,xStart ,yStart 1))))
      (if (not (null? stack))
      (begin
        (for-each (lambda (cell)
          (let ((x (first cell)) (y (second cell))
                (costMov (third cell)) (cost (third (car stack))))
            (when (> (array-ref dijkstraMap x y) (+ cost costMov))
              (set! stack (append stack `((,x ,y ,(+ cost costMov)))))
              (array-set! dijkstraMap x y (+ cost costMov)))))
          (neighbours dungeon width height (first (car stack)) (second (car stack))))
        (loop (cdr stack))))))


  (define (neighbours dungeon width height xStart yStart)
    (let ((cells '()))
      (for-each (lambda (cell)
        (let ((x (first cell)) (y (second cell)))
          (when (eq? (array-ref dungeon x y) #\.)
            (when (and (>= x 0) (>= y 0) (< x width) (< y height))
              (let ((cost (if (or (= xStart x) (= yStart y)) 1 1)))
                (set! cells (cons `(,x ,y ,cost) cells)))))))
        `((,(- xStart 1) ,yStart)
          (,(+ xStart 1), yStart)
          (,xStart ,(- yStart 1))
          (,xStart ,(+ yStart 1))))
      cells))


  (define (map-matrix x1 y1 x2 y2 f)
    (for-each (lambda (y)
      (for-each (lambda (x)
        (f x y))
        (iota (+ (- x2 x1) 1) x1 1)))
      (iota (+ (- y2 y1) 1) y1 1)))


  (define (calculatePath dijkstraMap width height x y)
    (let ((best (bestNear dijkstraMap width height x y)))
      (if (null? best)
      '()
      (cons `(,x ,y) (calculatePath dijkstraMap width height (first best) (second best))))))


  (define (bestNear dijkstraMap width height x y)
    (let ((bestDist (array-ref dijkstraMap x y)) (bestCell '()))
      (map-matrix (- x 1) (- y 1) (+ x 1) (+ y 1)
        (lambda (x y)
          (when (and (>= x 0) (>= y 0) (< x width) (< y height))
            (let ((cellDist (array-ref dijkstraMap x y)))
              (when (< cellDist bestDist)
                (set! bestDist cellDist)
                (set! bestCell `(,x ,y)))))))
      bestCell))
)
