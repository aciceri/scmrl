(module dungeon
        (<dungeon>
         make-dungeon
         get-width
         get-height
         get-grid)

        (import scheme
                srfi-1
                srfi-25 ;for arrays
                (chicken random)
                coops
                coops-primitive-objects)

        ;;;this isn't the definitive place for this functions
        (define (random-between a b)
          (+ (pseudo-random-integer (abs (- (+ b 1) a))) a)) ;random integer in [a, b]

        (define (map-matrix x1 y1 x2 y2 f) ;apply f for every (x, y) in the rectangle (x1, x2, y1, y2)
          (for-each (lambda (y)
                      (for-each (lambda (x)
                                  (f x y))
                                (iota (abs (- x1 x2)) (min x1 x2) 1)))
                    (iota (abs (- y1 y2)) (min y1 y2) 1)))

        (define WALL-CHAR #\#)
        (define FLOOR-CHAR #\.)

        (define-class <dungeon> ()
          ((grid initform: '() reader: get-grid writer: set-grid)
           (grid-width initform: 0 reader: get-width writer: set-width)
           (grid-height initform: 0 reader: get-height writer: set-height)
           (rooms initform: '() reader: get-rooms writer: set-rooms)
           (n-rooms initform: 0 reader: get-n-rooms writer: set-n-rooms)))

        (define (make-dungeon width height n-rooms n-cuts prob)
          (let ((d (make <dungeon>)))
            (begin
             (set-grid d (make-array (shape 0 width 0 height) WALL-CHAR))
             (set-width d width)
             (set-height d height)
             (set-n-rooms d n-rooms)
             (place-random-rooms d) ;place some random rooms
             (gen-maze d) ;generate a maze in the space between the rooms
             (connect-rooms d) ;create a single passage for every room
             (cut-maze d n-cuts) ;erase dead ends of the maze
             (lite-grid d prob) ;create some extra passages given a probability
              d)))

        (define-method (place-random-rooms (d <dungeon>)) ;place n random rooms (not overlapping) in the dungeon
          (define (dig-room x-start y-start width height)
            (map-matrix x-start y-start
                        (+ x-start width) (+ y-start height)
                        (lambda (x y)
                          (array-set! (get-grid d) x y FLOOR-CHAR))))

          (define (space-for-room? x-start y-start width height) ;check if there is space for the room in the dungeon
            (let ((is-ok #t))
              (map-matrix (- x-start 2) (- y-start 2)
                          (+ x-start width 2) (+ y-start height 2)
                          (lambda (x y)
                            (if (or (>= x (get-width d))
                                    (= x 0)
                                    (>= y (get-height d))
                                    (= y 0))
                                (set! is-ok #f)
                                (if (eq? (array-ref (get-grid d) x y) FLOOR-CHAR)
                                  (set! is-ok #f)))))
              is-ok))

          (define (random-room m) ;return a random room
            (let ((x (random-between 4 (- (get-width d) 12)))
                  (y  (random-between 4 (- (get-height d) 12)))
                  (width  (random-between 4 8))
                  (height  (random-between 4 8)))
              (if (space-for-room? x y width height)
                  (list x y width height)
                  (random-room m)))) ;it can loop here if it don't find space

          (set-rooms d (fold (lambda (i acc)
                               (let ((room (random-room (get-grid d))))
                                 (begin
                                   (apply dig-room room)
                                   (cons room acc))))
                             '()
                             (iota (get-n-rooms d))))
          )


        (define-method (count-neighbours (d <dungeon>) (x <integer>) (y <integer>))
          (let ((n 0)
                (m (get-grid d)))
            (begin
              (if (eq? (array-ref m x (+ y 1)) FLOOR-CHAR)
                  (set! n (+ n 1)))
              (if (eq? (array-ref m x (- y 1)) FLOOR-CHAR)
                  (set! n (+ n 1)))
              (if (eq? (array-ref m x (+ y 1)) FLOOR-CHAR)
                  (set! n (+ n 1)))
              (if (eq? (array-ref m x (- y 1)) FLOOR-CHAR)
                  (set! n (+ n 1)))
              n)))

        (define-method (gen-maze (d <dungeon>))
          (define (get-wall-random-coord m width height) ;return random coordinates a cell without floor
            (let ((x (random-between 3 (- width 3))) (y (random-between 3 (- height 3))))
              (if (and (eq? (array-ref m x y) WALL-CHAR) (= (count-neighbours d x y) 0))
                  (list x y)
                  (get-wall-random-coord m width height))))

          (define (dig-maze m stack width height) ;generate a maze using a recursive backtracker method
            (if (eq? stack '())
                '() ;if the maze is terminated
                (begin
                  (array-set! m (first (car stack)) (second (car stack)) FLOOR-CHAR)
                  (let ((new-coord (get-new-coord m (car stack) width height)))
                    (if (eq? new-coord '())
                        (dig-maze m (cdr stack) width height)
                        (dig-maze m (cons new-coord stack) width height))))))

          (define (get-new-coord m coord width height) ;get a new coordinate for the maze digger
            (let ((x (first coord)) (y (second coord)))
              (if (or (<= x 1) (<= y 1) (>= x (- width 2)) (>= y (- height 2)))
                  '()
                  (let ((directions '()))
                    (begin
                      (if (placeable? m x y 'e)
                        (set! directions (cons 'e directions)))
                      (if (placeable? m x y 'w)
                        (set! directions (cons 'w directions)))
                      (if (placeable? m x y 's)
                        (set! directions (cons 's directions)))
                      (if (placeable? m x y 'n)
                        (set! directions (cons 'n directions)))
                      (if (eq? directions '())
                          '()
                          (let ((dir (list-ref directions (random-between 0 (- (length directions) 1)))))
                            (cond ((eq? dir 'e) (list (+ x 1) y))  
                                  ((eq? dir 'w) (list (- x 1) y))
                                  ((eq? dir 's) (list x (+ y 1)))
                                  ((eq? dir 'n) (list x (- y 1)))))))))))

          (define (placeable? m x y dir) ;check if a cell can be placed to generate a maze's corridor
            (let ((is-ok #t))
              (cond ((eq? dir 'e)
                     (if (or (eq? (array-ref m (+ x 1) y) FLOOR-CHAR)
                               (eq? (array-ref m (+ x 1) (+ y 1)) FLOOR-CHAR)
                               (eq? (array-ref m (+ x 1) (- y 1)) FLOOR-CHAR)
                               (eq? (array-ref m (+ x 2) y) FLOOR-CHAR)
                               (eq? (array-ref m (+ x 2) (+ y 1)) FLOOR-CHAR)
                               (eq? (array-ref m (+ x 2) (- y 1)) FLOOR-CHAR))
                       (set! is-ok #f)))
                    ((eq? dir 'w)
                     (if (or (eq? (array-ref m (- x 1) y) FLOOR-CHAR)
                               (eq? (array-ref m (- x 1) (+ y 1)) FLOOR-CHAR)
                               (eq? (array-ref m (- x 1) (- y 1)) FLOOR-CHAR)
                               (eq? (array-ref m (- x 2) y) FLOOR-CHAR)
                               (eq? (array-ref m (- x 2) (+ y 1)) FLOOR-CHAR)
                               (eq? (array-ref m (- x 2) (- y 1)) FLOOR-CHAR))
                       (set! is-ok #f)))
                    ((eq? dir 's)
                     (if (or (eq? (array-ref m x (+ y 1)) FLOOR-CHAR)
                               (eq? (array-ref m (+ x 1) (+ y 1)) FLOOR-CHAR)
                               (eq? (array-ref m (- x 1) (+ y 1)) FLOOR-CHAR)
                               (eq? (array-ref m x (+ y 2)) FLOOR-CHAR)
                               (eq? (array-ref m (+ x 1) (+ y 2)) FLOOR-CHAR)
                               (eq? (array-ref m (- x 1) (+ y 2)) FLOOR-CHAR))
                       (set! is-ok #f)))
                    ((eq? dir 'n)
                     (if (or (eq? (array-ref m x (- y 1)) FLOOR-CHAR)
                               (eq? (array-ref m (+ x 1) (- y 1)) FLOOR-CHAR)
                               (eq? (array-ref m (- x 1) (- y 1)) FLOOR-CHAR)
                               (eq? (array-ref m x (- y 2)) FLOOR-CHAR)
                               (eq? (array-ref m (+ x 1) (- y 2)) FLOOR-CHAR)
                               (eq? (array-ref m (- x 1) (- y 2)) FLOOR-CHAR))
                       (set! is-ok #f))))
              is-ok))


          (dig-maze (get-grid d)
                    (list (get-wall-random-coord (get-grid d) (get-width d) (get-height d)))
                    (get-width d) (get-height d)))

        (define-method (connect-rooms (d <dungeon>))
          (define (connect-room m room attempt width height) ;create a floor cell connecting the room, may fail (nothing happens)
            (let ((room-x (first room)) (room-y (second room))
                  (room-width (third room)) (room-height (fourth room))
                  (dir 3) (door-x 0) (door-y 0))
              (begin
                (cond ((= dir (random-between 0 3))
                       (begin
                         (set! door-y (random-between room-y (+ room-y room-height)))
                         (set! door-x (+ room-x room-width))))
                      ((= dir 1)
                       (begin
                         (set! door-y (random-between room-y (+ room-y room-height)))
                         (set! door-x (- room-x 1))))
                      ((= dir 2)
                       (begin
                         (set! door-y (- room-y 1))
                         (set! door-x (random-between room-x (+ room-x room-width)))))
                      ((= dir 3)
                       (begin
                         (set! door-y (+ room-y room-height))
                         (set! door-x (random-between room-x (+ room-x room-width))))))
                (if (= (count-neighbours d door-x door-y) 2)
                    (array-set! m door-x door-y FLOOR-CHAR)
                    (if (> attempt 0)
                      (connect-room m room (- attempt 1) width height))))))

          (for-each (lambda (r)
             (connect-room (get-grid d) r 10 (get-width d) (get-width d)))
           (get-rooms d)))

        (define-method (cut-maze (d <dungeon>) (n <integer>))
          (for-each (lambda (i)
                      (map-matrix 0 0 (get-width d) (get-height d)
                                  (lambda (x y)
                                    (if (and
                                           (eq? (array-ref (get-grid d) x y) FLOOR-CHAR)
                                           (= (count-neighbours d x y) 1))
                                      (array-set! (get-grid d) x y WALL-CHAR)
                                      ))))
                    (iota n)))

        (define-method (lite-grid (d <dungeon>) (prob <integer>))
          (map-matrix 1 1 (- (get-width d) 1) (- (get-height d) 1)
                      (lambda (x y)
                        (if (< (random-between 0 99) prob)
                          (let ((m (get-grid d)))
                            (if (and (eq? (array-ref m (+ x 1) y) FLOOR-CHAR)
                                       (eq? (array-ref m x y) WALL-CHAR)
                                       (eq? (array-ref m (- x 1) y) FLOOR-CHAR))
                              (array-set! m x y FLOOR-CHAR))
                            (if (and (eq? (array-ref m x (+ y 1)) FLOOR-CHAR)
                                       (eq? (array-ref m x y) WALL-CHAR)
                                       (eq? (array-ref m x (- y 1)) FLOOR-CHAR))
                              (array-set! m x y FLOOR-CHAR)))))))

        )
