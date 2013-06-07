#lang racket

(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) (+ x 1))

((double inc) 5) ;7

(((double (double double)) inc) 5)
; (double (double double)) inc = (double double) ((double double) (inc)) = (double double) (double (double inc))
; ((double double) inc) = (double (double inc)) = 2*2
; (double double) (double (double inc)) = 2*2*2*2 = 16
; so 5+16 = 21

