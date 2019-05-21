(module player
        (<player>
         make-player
         get-x
         get-y
         move)

        (import scheme
                coops
                coops-primitive-objects)

        (define-class <player> ()
          ((name reader: get-name writer: set-name)
           (x reader: get-x writer: set-x)
           (y reader: get-y writer: set-y)))

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


        )
