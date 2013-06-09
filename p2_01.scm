#lang racket

(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (> (* n d) 0)
        (cons (/ (abs n) g) (/ (abs d) g))
        (cons (/ (- 0 (abs n)) g) (/ (abs d) g)))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(print-rat (make-rat 12 15))   ;4/5
(print-rat (make-rat -12 15))  ;-4/5
(print-rat (make-rat 12 -15))  ;-4/5
(print-rat (make-rat -12 -15)) ;4/5