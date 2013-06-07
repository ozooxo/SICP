#lang racket

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;;;

(define (filtered-accumulate combiner null-value filter term a next b)
  (if (> a b)
      null-value
      (combiner (if (filter a) (term a) null-value)
         (filtered-accumulate combiner null-value filter term (next a) next b))))

;;;

(define (sum-prime-integers a b)
  (define (identity x) x)
  (define (inc n) (+ n 1))
  (filtered-accumulate + 0 prime? identity a inc b))

(sum-prime-integers 1 10) ;18
(+ 1 2 3 5 7) ;18

(define (sum-square-prime-integers a b)
  (define (inc n) (+ n 1))
  (filtered-accumulate + 0 prime? square a inc b))

(sum-square-prime-integers 1 10) ;88