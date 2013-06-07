#lang racket

(define (cont-frac n d k)
  (define (cont-frac-tmp n d k) 
    (if (< k 2)
        (/ (n 0) (d 0))
        (/ (n k) (+ (d k) (cont-frac-tmp n d (- k 1))))))
  (cont-frac-tmp (lambda (i) (n (- (+ k 1) i))) (lambda (i) (d (- (+ k 1) i))) k))

(define (Ni x) (lambda (i) (if (= i 1)
                               x
                               (* x x -1))))

(define Di (lambda (i) (- (* 2 i) 1)))

(cont-frac (Ni (/ pi 6)) Di 1000) ;0.5773502691896257
(tan (/ pi 6))

(cont-frac (Ni (/ pi 12)) Di 1000) ;0.26794919243112264
(tan (/ pi 12))

