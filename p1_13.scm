#lang racket

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

;;;

(define (pow x n)
  (exp (* n (log x))))

(define phi (/ (+ 1 (sqrt 5)) 2))

(define psi (/ (- 1 (sqrt 5)) 2))

(define (fib-appro n)
  (/ (- (pow phi n) (pow psi n)) (sqrt 5)))

;;;

(fib 15) ;610

(fib-appro 15)

;PROOF
; phi*psi = -1
; f(n-1) + f(n-2) = -phi^(n-2)*(3+sqrt(5))/(2*sqrt(5)) + psi^(n-2)*(3-sqrt(5))/(2*sqrt(5))
;                 = -phi^(n-1)*(-1/sqrt(5))*phi + psi^(n-1)*(-1/sqrt(5))*psi = f(n)

(/ psi (sqrt 5))
; -0.27 < psi^n/sqrt(5) < 0 thus closest integer. QED
