#lang racket

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f k)
  (if (= k 1)
      f
      (compose f (repeated f (- k 1)))))

(define (smooth f)
  (define dx 0.001)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (n-fold-smooth n)
  (repeated smooth n))

((smooth sin) (/ pi 2)) ;0.9999996666666945

(((n-fold-smooth 5) sin) (/ pi 2)) ;0.9999983333345833