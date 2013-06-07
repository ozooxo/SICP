#lang racket

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))    

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
(show-howmany-prime 1000000 3)

;101 *** 0.009033203125
;103 *** 0.009033203125
;107 *** 0.010009765625
;1009 *** 0.013916015625
;1013 *** 0.01318359375
;1019 *** 0.01220703125
;10007 *** 0.01806640625
;10009 *** 0.01513671875
;10037 *** 0.014892578125
;1000003 *** 0.030029296875 ; Around two times the time-consuming of ~1000 primes.
;1000033 *** 0.02685546875
;1000037 *** 0.037841796875