#lang racket

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p))) ; (lambda (p q) p) will be the function which replace m, and ((lambda (p q) p) x y) = x

(define (cdr z)
  (z (lambda (p q) q)))

(car (cons 1 2)) ;1
(cdr (cons 1 2)) ;2
; don't understand why the book says "To verify that this works, make use of the substitution model of section 1.1.5."