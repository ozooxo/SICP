#lang racket

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (define (identity x) x)
  (define (inc n) (+ n 1))
  (product identity 1 inc n))

(factorial 5) ;120

;;;

(define (pi-term x)
  (define (square x) (* x x))
  (/ (* x (+ x 2)) (square (+ x 1))))

(define (pi-product n)
  (define (inc-2 n) (+ n 2))
  (product pi-term 2 inc-2 n))

(* 4.0 (pi-product 10000)) ;3.1417497057380523