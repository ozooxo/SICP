#lang racket

(define (iterative-improve good-enough? improve)
  (define (f guess x) (if (good-enough? guess x)
                          guess
                          (f (improve guess x) x)))
  (lambda (guess x) (f guess x))) 

;;;

(define (sqrt x)
  (define (good-enough? guess x)
    (define (square x) (* x x))
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    (define (average x y) (/ (+ x y) 2))
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0 x))

(sqrt 2) ;1.4142156862745097

;;;

(define (fixed-point f first-guess)
  (define (close-enough? x f)
    (< (abs (- x (f x))) 0.00001))
  (define (next x f) (f x))
  ((iterative-improve close-enough? next) first-guess f))

(fixed-point cos 1.0) ;0.7390893414033927