(use coops)


(define-class <Scheduler> ()
  ((ticks initform: 0)  ;ticks start from 0 by default
   (events '())))

(define-class <Event> ()
  (name func ticksDelay startTick))  ;name is only for debug purpose

(define-class <Actor> ()
  (name))


(define-method (progress (scheduler <Scheduler>))  ;progress the ticks i.e. go in the future
  (set! (slot-value scheduler 'ticks) (+ (slot-value scheduler 'ticks) 1))
  (let ((newEvents '()))  ;because some events may be deleted (if they're past)
    (for-each (lambda (event)
               (if (= (slot-value event 'startTick) (slot-value scheduler 'ticks))
                 ((slot-value event 'func))  ;evalutate 'func if it's the time
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
                 'func (lambda () (format #t "ciao 1\n"))
                 'ticksDelay 2))

(define e2 (make <Event>
                 'name "Event 2"
                 'func (lambda () (format #t "ciao 2\n"))
                 'ticksDelay 5))

(addEvent s e1)
(addEvent s e2)

