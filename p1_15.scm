#lang racket

;PROB a
(define (count x n)
  (if (< x 0.1)
      n
      (count (/ x 3) (+ n 1))))

(count 12.15 0) ;5

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

;PROB b
; roughly 4^n