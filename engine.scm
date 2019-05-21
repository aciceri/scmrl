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
                (dungeon (make-dungeon 100 70 14 0 0)))
            (loop terminal player dungeon)
            ))

        (define-method (loop (term <terminal>) (player <player>) (dungeon <dungeon>))
          (let repeat ((key (get-key term)))
            (push-msg term (format #f "~a" key))

            (cond
             ((eq? key #\w) (move player 'n))
             ((eq? key #\a) (move player 'w))
             ((eq? key #\s) (move player 's))
             ((eq? key #\d) (move player 'e)))


            (draw-map term dungeon player)
            (if (not (eq? key #\q))
                (repeat (get-key term))
                (destroy-terminal term "Correctly terminated")
           )))


        )
