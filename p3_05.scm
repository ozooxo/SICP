#lang racket

(define (rand-update x)
  (begin
    (random-seed x)
    (random 10)))

(define rand
  (let ([x 1])
    (lambda ()
      (set! x (rand-update x))
      x)))

;The above procedure behaves the "rand" in the texbook. However, it is not directly related to Exercise 3.5.;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random) range)))) ;I changed it from "(+ low (random range))))" as in the textbook.
                                 ;So right now it gives a float in the desired region.

(define (estimate-integral P x1 x2 y1 y2 trials)
  (let ([count-true 0] [count-false 0])
    (define (if-in-P trials)
      (if (= trials 0)
          (* (/ count-true (+ count-true count-false)) (- x2 x1) (- y2 y1) 1.0)
          (begin (if (P (random-in-range x1 x2) (random-in-range y1 y2))
                     (set! count-true (+ count-true 1))
                     (set! count-false (+ count-false 1)))
                 (if-in-P (- trials 1)))))
    (if-in-P trials)))

(define (P x y)
  (define (square x) (* x x))
  (not (> (+ (square (- x 5)) (square (- y 7))) (square 3))))

(estimate-integral P 2 8 4 10 1000) ;28.44
(* pi 3 3) ;28.274333882308138