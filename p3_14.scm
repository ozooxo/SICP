#lang racket

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (mcdr x)))
          (set-mcdr! x y)
          (loop temp x))))
  (loop x '()))

(define v (mcons 'a (mcons 'b (mcons 'c (mcons 'd '())))))
(define w (mystery v))
w ;(mcons 'd (mcons 'c (mcons 'b (mcons 'a '()))))
  ;It is just the reverse of v.