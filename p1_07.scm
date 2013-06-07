#lang racket

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)
  ))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

;(define (good-enough? guess x)
;  (< (abs (- (square guess) x)) 0.01))

(define (good-enough? guess x)
  (< (/ (abs (- (square guess) x)) x) 0.01))

(define (square x) (* x x))

;(sqrt-iter 1.0 9)
(sqrt-iter 1.0 0.0001) ;0.06303035962394365 ;0.010000714038711746
