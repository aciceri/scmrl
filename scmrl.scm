(import terminal)
(import dungeon)
(import coops)
(import srfi-28)

(define t (make-terminal))
(define d (make-dungeon 100 70 14 0 0))
(draw-map 20 10 t d)
(get-key t)
(push-msg t (format #f "~a ~a" (get-width d) (get-height d)))
(get-key t)
(define name (ask-msg t "come ti chiami? " 4))
(push-msg t name)
(get-key t)
(destroy-terminal t "Bye")
