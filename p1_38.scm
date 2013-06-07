#lang racket

(define (cont-frac n d k) 
  (if (< k 2)
      (/ (n 0) (d 0)) ; In here, the very bottom one is n_0, d_0, where the top one is n_k, t_k.
                      ; It is not exactly the same as the equation defined. But for "(lambda (i) 1.0)", it is fine.
      (/ (n k) (+ (d k) (cont-frac n d (- k 1))))))

(define Ni (lambda (i) 1.0))

(define (Di k) (lambda (i-new)
                 (let ((i (- (+ k 1) i-new))) ; This tedious index rename is aimed to flip the sign of the index in "d, k" defined in (cont-frac n d k)
                   (if (or (= (remainder i 3) 0) (= (remainder i 3) 1))
                       1
                       (* (/ (+ i 1) 3) 2)))))

(+ (cont-frac Ni
           (Di 1000)
           1000) 2)

(exp 1)

