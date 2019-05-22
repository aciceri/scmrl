(module engine
        (make-engine
         start)

        (import scheme
                coops
                terminal
                player
                dungeon
                srfi-28)

        (define-class <engine> ()
          ((foo initform: '())))

        (define (make-engine)
          (let ((e (make <engine>)))
            e))

        (define-method (start (e <engine>))
          (let ((terminal (make-terminal))
                (player (make-player))
                (dungeon (make-dungeon 80 40 20 0 0)))
            (loop terminal player dungeon)
            ))

        (define-method (loop (term <terminal>) (player <player>) (dungeon <dungeon>))
          (let repeat ((key (get-key term)))
            (push-msg term (format #f "You pressed \"~a\"" key))

            (cond
             ((eq? key #\w) (move player 'n))
             ((eq? key #\a) (move player 'w))
             ((eq? key #\s) (move player 's))
             ((eq? key #\d) (move player 'e)))

            (update-fov player dungeon)

            (draw-map term dungeon player)

            (if (not (eq? key #\q))
                (repeat (get-key term))
                (destroy-terminal term "Correctly terminated")
           )))


        )
