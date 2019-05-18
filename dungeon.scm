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
          (+ (pseudo-random-integer (abs (- b a))) a))

        (define (map-matrix m x1 y1 x2 y2 f) ;apply f for every (x, y) in the rectangle (x1, x2, y1, y2)
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
             (set-width d height)
             (set-height d width)
             (set-n-rooms d n-rooms)
             (place-random-rooms d) ;place some random rooms
              ;(genMaze! grid `(,(getWallRandomCoord grid width height)) width height) ;generate a maze in the space between the rooms
              ;(connectRooms! grid rooms width height) ;create a single passage for every room
              ;(cutMaze! grid nCuts width height) ;erase dead ends of the maze
              ;(liteGrid! grid prob width height) ;create some extra passages given a probability
              d)))

        (define-method (place-random-rooms (d <dungeon>)) ;place n random rooms (not overlapping) in the dungeon
          (define (dig-room m x-start y-start width height)
            (map-matrix m x-start y-start
                        (+ x-start width) (+ y-start height)
                        (lambda (x y)
                          (array-set! m x y FLOOR-CHAR))))

          (define (space-for-room? m x-start y-start width height grid-width grid-height) ;check if there is space for the room in the dungeon
            (let ((is-ok #t))
              (map-matrix m (- x-start 2) (- y-start 2)
                          (+ x-start width 2) (+ y-start height 2)
                          (lambda (x y)
                            (if (or (>= x grid-width)
                                    (= x 0)
                                    (>= y grid-height)
                                    (= y 0))
                                (set! is-ok #f)
                                (if (eq? (array-ref m x y) FLOOR-CHAR)
                                  (set! is-ok #f)))))
              is-ok))

          (define (random-room m grid-width grid-height) ;return a random room
            (let ((x (random-between 4 (- grid-width 12)))
                  (y  (random-between 4 (- grid-height 12)))
                  (width  (random-between 4 8))
                  (height  (random-between 4 8)))
              (if (space-for-room? m x y width height grid-width grid-height)
                  (list x y width height)
                  (random-room m grid-width grid-height)))) ;it can loop here if it don't find space

          (set-rooms d (fold (lambda (i acc)
                               (let ((room (random-room (get-grid d) (get-width d) (get-height d))))
                                 (begin
                                   (display room)
                                   (apply dig-room (cons (get-grid d) room))
                                   (cons room acc))))
                             '()
                             (iota (get-n-rooms d))))
          )



        )
