#lang racket

(define (cube x) (* x x x))

(define (sum term a next b)
  (define (iter x result)
    (if (> x b)
        result
        (iter (next x) (+ result (term x)))))
  (iter a 0))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.01) ;0.24998750000000042
(integral cube 0 1 0.001) ;0.249999875000001
(integral cube 0 1 0.0001) ;0.24999999874993412

