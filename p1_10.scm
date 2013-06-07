#lang racket

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;(A 1 10) ;1024
;(A 2 4) ;65536

(define (pow x n)
  (if (= n 1)
      x
      (* x (pow x (- n 1)))))

;(pow 3 4) ;81

(define (powtower x n)
  (if (= n 1)
      x
      (pow x (powtower x (- n 1)))))

;(powtower 2 4) ;65536

(define (f n) (* 2 n))

(define (g n) (pow 2 n))

(define (h n) (powtower 2 n))

(define (k n) (* 5 n n))