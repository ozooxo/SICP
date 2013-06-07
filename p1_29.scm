#lang racket

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;;;

(define (integral-simpson f a b dx)
  (define (add-dx x) (+ x dx dx))
  (/ (+
      (* (sum f a add-dx b) dx)
      (* 4 (sum f (+ a dx) add-dx b) dx)
      (* (sum f (+ a dx dx) add-dx b) dx)
   ) 3.0))

(integral-simpson cube 0 1 0.01) ;0.24666666666666712
(integral-simpson cube 0 1 0.001) ;0.24966666666666748
(integral-simpson cube 0 1 0.0001) ;0.25003333333326755

;;;

(define (integral-simpson-2 f a b n)
  (define h (/ (+ a b) n))
  (define (add-dx x) (+ x h h))
  (/ (+
      (* (sum f a add-dx b) h)
      (* 4 (sum f (+ a h) add-dx b) h)
      (* (sum f (+ a h h) add-dx b) h)
   ) 3.0))

(integral-simpson-2 cube 0 1 100) ;0.25333333333333335
(integral-simpson-2 cube 0 1 1000) ;0.25033333333333335
(integral-simpson-2 cube 0 1 10000) ;0.25003333333333333

;;;

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.01) ;0.24998750000000042
(integral cube 0 1 0.001) ;0.249999875000001
(integral cube 0 1 0.0001) ;0.24999999874993412

; Simpson's method seems not really better than the original method. I understand that in Simpson's method, the effective dx is 2 times
; larger than the dx in the original method. However, this difference cannot really explain the huge inaccuracy rises from the Simpson's
; method. I don't understand why. (P.S. using n=101 is worse than n=100, so the effect of cutting half is not really significant.)