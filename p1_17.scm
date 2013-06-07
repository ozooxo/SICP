#lang racket

(define (even? n)
  (= (remainder n 2) 0))

(define (double x) (* x 2))

(define (halve x) (/ x 2))

(define (fast-multiply a b)
  (cond ((= b 1) a) 
        ((even? b) (fast-multiply (double a) (halve b)))
        (else (+ (fast-multiply a (halve (- b 1))) (fast-multiply a (halve (+ b 1)))))))

(fast-multiply 11 13) ;143