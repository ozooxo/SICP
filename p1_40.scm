#lang racket

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (square x) (* x x))

(define (deriv g)
  (define dx 0.00001)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))


(newtons-method (cubic 0 0 1) 1.0) ;-0.9999999999999863 -- should be +1, -1
(newtons-method (cubic 1 0 0) 1.0) ;1.1227429100448376e-05 -- should be -1, 0
(newtons-method (cubic 1 2 3) 1.0) ;-1.2756822036498454

((cubic 1 2 3) -1.2756822036498454) ;4.935607478273596e-12 should be 0