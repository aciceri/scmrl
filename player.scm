(module player
        (<player>
         make-player
         get-x
         get-y
         move
         get-fov
         update-fov)

        (import scheme
                coops
                coops-primitive-objects
                dungeon)

        (define-class <player> ()
          ((name reader: get-name writer: set-name)
           (x reader: get-x writer: set-x)
           (y reader: get-y writer: set-y)
           (fov reader: get-fov writer: set-fov)
           (explored reader: get-explored writer: set-explored)))

        (define (make-player)
          (let ((p (make <player>)))
            (set-x p 10)
            (set-y p 10)
            p))

        (define-method (move (p <player>) (dir <symbol>))

          (cond
           ((eq? dir 'n) (set-y p (- (get-y p) 1)))
           ((eq? dir 's) (set-y p (+ (get-y p) 1)))
           ((eq? dir 'w) (set-x p (- (get-x p) 1)))
           ((eq? dir 'e) (set-x p (+ (get-x p) 1))))

          )

        (define-method (update-fov (p <player>) (d <dungeon>))
          (set-fov p (fov d (get-x p) (get-y p) 4))) ;4 is the fov radius

        (define-method (update-explored (p <player>) (d <dungeon>))
          '()
          )


        )
