#lang racket

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(display "without average")
(newline)
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
(newline)

(display "with average")
(newline)
(define (average x y) (/ (+ x y) 2))
(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)

