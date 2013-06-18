#lang racket

(define (make-queue)
  (let ([front-ptr '()]
        [rear-ptr '()])
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue")
          (mcar front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (mcons item '())))
        (cond ((empty-queue?)
               (begin
                 (set! front-ptr new-pair)
                 (set! rear-ptr new-pair)))
              (else
               (set-mcdr! rear-ptr new-pair)
               (set! rear-ptr new-pair))))) 
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue"))
            (else
             (set! front-ptr (mcdr front-ptr))))) 
    (define (print-queue)
      (display front-ptr)
      (newline))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'print-queue) print-queue)
            (else (error "Unknown request -- MAKE-QUEUE" m))))
    dispatch))

(define q1 (make-queue))
((q1 'insert-queue!) 'a)
((q1 'print-queue)) ;{a}
((q1 'insert-queue!) 'b)
((q1 'print-queue)) ;{a b}
((q1 'delete-queue!))
((q1 'print-queue)) ;{b}
((q1 'delete-queue!))
((q1 'print-queue)) ;()