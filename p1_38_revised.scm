#lang racket

(define (cont-frac n d k)
  (define (cont-frac-tmp n d k) 
    (if (< k 2)
        (/ (n 0) (d 0))
        (/ (n k) (+ (d k) (cont-frac-tmp n d (- k 1))))))
  (cont-frac-tmp (lambda (i) (n (- (+ k 1) i))) (lambda (i) (d (- (+ k 1) i))) k))

(define Ni (lambda (i) 1.0))

(define Di (lambda (i) (if (or (= (remainder i 3) 0) (= (remainder i 3) 1))
                           1
                           (* (/ (+ i 1) 3) 2))))

(+ (cont-frac Ni Di 1000) 2) ;2.7182818284590455
(exp 1) ;2.718281828459045

