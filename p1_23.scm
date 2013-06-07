#lang racket

(define (square x) (* x x))

(define (next x)
  (if (= x 2) 3
      (+ x 2)))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;;;

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))
(define (start-prime-test n start-time)
  (cond ((prime? n)
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
        ((prime? n) (if-prime n howmany)) ;In here I calculate (prime? n) twice. There should be a better way.
        (else (if-not-prime n howmany))))

(timed-prime-test 5)

(show-howmany-prime 100 3)
(show-howmany-prime 1000 3)
(show-howmany-prime 10000 3)

;101 *** 0.002197265625
;103 *** 0.001953125
;107 *** 0.001953125
;1009 *** 0.0048828125
;1013 *** 0.004150390625
;1019 *** 0.00390625
;10007 *** 0.010009765625
;10009 *** 0.010009765625
;10037 *** 0.009033203125

; For the time difference compare to p1_22.scm:
; "0.001953125" appear several times. It may rises from the function "(current-inexact-milliseconds)" in an unexpect way.
; For ~1000 and ~10000 primes, the new method is around 1.4 times faster. It is not exactly two times, maybe because although
; the original method test more divisors, the new method need to test whether divisor=2 everytime (which is also time consuming).
; I don't know where the exact factor 1.4 comes from.