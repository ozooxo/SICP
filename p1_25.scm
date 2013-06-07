#lang racket

(define (square x) (* x x))

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;;;

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))
(define (start-prime-test n start-time)
  (cond ((fast-prime? n 3)
      (report-prime (- (current-inexact-milliseconds) start-time)))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (if-prime n howmany)
  (timed-prime-test n)
  (show-howmany-prime (+ n 1) (- howmany 1)))

(define (if-not-prime n howmany)
  (show-howmany-prime (+ n 1) howmany))

(define (show-howmany-prime n howmany)
  (cond ((= howmany 0) (display ""))
        ((fast-prime? n 3) (if-prime n howmany)) ;In here I calculate (fast-prime? n 3) twice. There should be a better way.
                                                 ;There is a tiny possibility that the two (fast-prime? n 3) run different result.
        (else (if-not-prime n howmany))))

(timed-prime-test 5)

(show-howmany-prime 100 3)
(show-howmany-prime 1000 3)
(show-howmany-prime 10000 3)

;101 *** 0.01611328125
;103 *** 0.01708984375
;107 *** 0.015869140625
;1009 *** 0.072998046875
;1013 *** 0.072021484375
;1019 *** 0.06298828125
;10007 *** 4.0869140625
;10009 *** 3.566162109375
;10037 *** 3.166015625
; It seems much slower, especially for larger prime numbers.
; The reason is that (LHS expmod, RHS Alyssa P. Hacker)
; (base^exp)%m = ((base^(exp/2))^2)%m = (base^(exp/2)%m)^2%m
; (base^exp)%m = (base*base^(exp-1))%m = (base*(base^(exp-1)%m))%m
; Therefore, the method of Alyssa P. Hacker need to face extremely large numbers ~10000^sqrt(10000), while the numbers
; in original expmod never exceed ~10000. That's the reason the original expmod is much quicker than Alyssa P. Hacker.