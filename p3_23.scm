#lang racket

(define (front-ptr deque) (mcar deque))
(define (rear-ptr deque) (mcdr deque))
(define (set-front-ptr! deque item) (set-mcar! deque item))
(define (set-rear-ptr! deque item) (set-mcdr! deque item))

(define (empty-deque? deque) (and (null? (front-ptr deque)) (null? (rear-ptr deque))))

(define (make-deque) (mcons '() '()))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (mcar (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (mcar (rear-ptr deque))))

(define (front-insert-deque! deque item)
  (let ([new-pair (mcons item (mcons '() '()))])
    (cond ((empty-deque? deque)
           (begin
             (set-front-ptr! deque new-pair)
             (set-rear-ptr! deque new-pair)
             deque))
          (else
           (begin
             (set-mcar! (mcdr (front-ptr deque)) new-pair)
             (set-mcdr! (mcdr new-pair) (front-ptr deque))
             (set-front-ptr! deque new-pair)
             deque)))))

(define (rear-insert-deque! deque item)
  (let ([new-pair (mcons item (mcons '() '()))])
    (cond ((empty-deque? deque)
           (begin
             (set-front-ptr! deque new-pair)
             (set-rear-ptr! deque new-pair)
             deque))
          (else
           (begin
             (set-mcdr! (mcdr (rear-ptr deque)) new-pair)
             (set-mcar! (mcdr new-pair) (rear-ptr deque))
             (set-rear-ptr! deque new-pair)
             deque)))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        ((eq? (front-ptr deque) (rear-ptr deque))
         (begin
           (set-front-ptr! deque '())
           (set-rear-ptr! deque '())
           deque))
        (else
         (begin
           (set-front-ptr! deque (mcdr (mcdr (front-ptr deque))))
           (set-mcar! (mcdr (front-ptr deque)) '())
           deque))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        ((eq? (front-ptr deque) (rear-ptr deque))
         (begin
           (set-front-ptr! deque '())
           (set-rear-ptr! deque '())
           deque))
        (else
         (begin
           (set-rear-ptr! deque (mcar (mcdr (rear-ptr deque))))
           (set-mcdr! (mcdr (rear-ptr deque)) '())
           deque))))

(define (print-deque deque)
  (define (print-deque-tmp pointer)
    (cond ((empty-deque? deque) (display "empty deque"))
          ((eq? (mcdr (mcdr pointer)) '()) (display (mcar pointer)))
          (else (begin
                  (display (mcar pointer))
                  (print-deque-tmp (mcdr (mcdr pointer)))))))
  (begin 
    (print-deque-tmp (front-ptr deque))
    (newline)))

(define q1 (make-deque))
(front-insert-deque! q1 'a)
(print-deque q1) ;a
(front-insert-deque! q1 'b)
(print-deque q1) ;ba
(rear-insert-deque! q1 'c)
(print-deque q1) ;bac
(front-delete-deque! q1)
(print-deque q1) ;ac
(rear-delete-deque! q1)
(print-deque q1) ;a
(rear-delete-deque! q1)
(print-deque q1) ;empty deque