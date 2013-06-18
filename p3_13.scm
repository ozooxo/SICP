#lang racket

(define (make-cycle x)
  (set-mcdr! (last-pair x) x)
  x)

(define (last-pair x)
  (if (null? (mcdr x))
      x
      (last-pair (mcdr x))))

(define z (make-cycle (mcons 'a (mcons 'b (mcons 'c '())))))

z ;#0=(mcons 'a (mcons 'b (mcons 'c #0#)))

(last-pair z) ;It will run forever.