(use coops)


(define-class <Scheduler> ()
  ((ticks initform: 0)
   (events '())))

(define-class <Event> ()
  (name ticksDelay startTick))

(define-class <Actor> ()
  (name))


(define-method (progress (scheduler <Scheduler>))
  (set! (slot-value scheduler 'ticks) (+ (slot-value scheduler 'ticks) 1))
  (let ((newEvents '()))
    (for-each (lambda (event)
               (if (= (slot-value event 'startTick) (slot-value scheduler 'ticks))
                 (display (slot-value event 'name))
                 (set! newEvents (cons event newEvents))) 
             ) (slot-value scheduler 'events))))


(define-method (addEvent (scheduler <Scheduler>) (event <Event>))
  (set! (slot-value event 'startTick) (+ (slot-value scheduler 'ticks)
                                         (slot-value event 'ticksDelay)))
  (set! (slot-value scheduler 'events) (cons event
                                             (slot-value scheduler 'events))))


(define s (make <Scheduler>))
(define e1 (make <Event>
                 'name "Event 1"
                 'ticksDelay 2))

(define e2 (make <Event>
                 'name "Event 2"
                 'ticksDelay 5))

(addEvent s e1)
(addEvent s e2)

