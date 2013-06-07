#lang racket

(define (even? n)
  (= (remainder n 2) 0))

(define (double x) (* x 2))

(define (halve x) (/ x 2))

(define (fast-multiply-tmp a b c)
  (cond ((= b 1) (+ a c)) 
        ((even? b) (fast-multiply-tmp (double a) (halve b) c))
        (else (fast-multiply-tmp (double a) (halve (- b 1)) (+ a c)))))

(define (fast-multiply a b)
  (fast-multiply-tmp a b 0))

(fast-multiply 11 13) ;143
