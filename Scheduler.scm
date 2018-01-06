(module Scheduler
    ((addActor)
    (tick))
  (import chicken scheme)
  (use srfi-1)


  (define actors '())

  (define (addActor actionPoints callback)
    (set! actors (append! actors `((,actionPoints ,actionPoints ,callback)))))

  (define (tick)
    (when (not (null? actors))
      (let* ((actor (car actors))
             (maxAP (car actor)) (actualAP (cadr actor)) (callback (caddr actor)))
        (if (= (- actualAP 1) 0)
          (begin
            (set! actors (append! actors `((,maxAP ,maxAP ,callback))))
            (callback))
          (set! actors (append! actors `((,maxAP ,(- actualAP 1) ,callback)))))
        (set! actors (cdr actors)))))

)
