#lang racket

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (accumulate combiner null-value term (next a) next b))))

;;;

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (cube x) (* x x x))

(integral cube 0 1 0.01) ;0.24998750000000042

;;;

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (factorial n)
  (define (identity x) x)
  (define (inc n) (+ n 1))
  (product identity 1 inc n))

(factorial 5) ;120