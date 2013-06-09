#lang racket

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (mod-times x a)
  (define (mod-times-recr x a n)
    (if (= (remainder x a) 0)
        (mod-times-recr (/ x a) a (+ n 1))
        n))
  (mod-times-recr x a 0))

;(mod-times 24 2) ;3

(define (car n)
  (mod-times n 2))

(define (cdr n)
  (mod-times n 3))

(car (cons 14 20)) ;14
(cdr (cons 14 20)) ;20