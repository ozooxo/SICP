#lang racket

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f k)
  (if (= k 1)
      f
      (compose f (repeated f (- k 1)))))

(define (square x) (* x x))
((repeated square 2) 5) ;625