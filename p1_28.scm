#lang racket

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((and (not (= base 1)) (not (= base (- m 1))) (= (remainder (* base base) m) 1)) 
         ;(display "special")
         0)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))    

(define (fermat-test n)
  (define (try-it a)
    (cond ((= (expmod  a (- n 1) n) 1) true)
          (else false)))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(fast-prime? 17 5) ;#t
(fast-prime? 15 5) ;#f
; I have tested several times. Currently "(fast-prime? 15 5)" is always false.
; If turns on "(display "special")", it can be seen that sometimes "fast-prime?" turn false because "expmod" returns zero.