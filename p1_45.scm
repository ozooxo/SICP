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

(define (average-damp f)
  (define (average x y) (/ (+ x y) 2))
  (lambda (x) (average x (f x))))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f k)
  (if (< k 2)
      f
      (compose f (repeated f (- k 1)))))

;;;

(define (nth-root-tmp x n damping-k)
  (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1))))
                            (repeated average-damp damping-k)
                            1.0))

(nth-root-tmp 2 2 1)
(nth-root-tmp 2 3 1)
(nth-root-tmp 2 4 2)
(nth-root-tmp 2 5 2)
(nth-root-tmp 2 6 2)
(nth-root-tmp 2 7 2)
(nth-root-tmp 2 8 3)
; So it seems that the required damping number is the integer smaller than (log_2 n).

(define (nth-root x n)
  (nth-root-tmp x n (/ (log n) (log 2))))

(newline)
(nth-root 2 2)
(nth-root 2 3)
(nth-root 2 4)
(nth-root 2 5)
(nth-root 2 6)
(nth-root 2 7)
(nth-root 2 8)