#lang racket

(define (make-accumulator base-amount)
  (lambda (add-amount)
    (begin
      (set! base-amount (+ base-amount add-amount))
      base-amount)))

(define A (make-accumulator 5))
(A 10) ;15
(A 10) ;25