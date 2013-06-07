#lang racket

(define (square x) (* x x))

(define (f g)
  (g 2))

(f square) ;4
(square 2) ;4

(f (lambda (z) (* z (+ z 1)))) ;6
((lambda (z) (* z (+ z 1))) 2) ;6

; Evaluation of (f f)~(f 2), but f is not defined, so (f 2) cannot be evaluated.