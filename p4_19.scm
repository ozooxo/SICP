#lang racket

(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))

;Racket gives "#<undefined>" error.

;I believe that Chapter 4.1.4 will give 16 (b=11, a=5); Chapter 4.1.5/Exercise 4.16 will give undefined error.
;So both Ben and Alyssa are right.

;I believe there is no general+easy way to solve this problem (especially, multiple assignments don't work).
;One possible way to solve it, is to treat the two assignments as two equations {b = a+x, a = 5} and solve for {a, b}.
;We may use the algorithm similar to Chapter 3.3.5 (solve linear equations) to handle this problem.