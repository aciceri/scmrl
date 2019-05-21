(module utils
        (random-between
         map-matrix)

        (import scheme
                (chicken random)
                srfi-1)

        (define (random-between a b)
          (+ (pseudo-random-integer (abs (- (+ b 1) a))) a)) ;random integer in [a, b]

        (define (map-matrix x1 y1 x2 y2 f) ;apply f for every (x, y) in the rectangle (x1, x2, y1, y2)
          (for-each (lambda (y)
                      (for-each (lambda (x)
                                  (f x y))
                                (iota (abs (- x1 x2)) (min x1 x2) 1)))
                    (iota (abs (- y1 y2)) (min y1 y2) 1)))


        )
