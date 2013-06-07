#lang racket

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
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

;(current-inexact-milliseconds)
(timed-prime-test 5)
;If we don't run something before "(show-howmany-prime 100 3)", the time for the first prime after 100 will be extremely large.
;If we only run "(current-inexact-milliseconds)" one or more times before run "show-howmany-prime", the times varie.
;I don't exactly understand why. "start-time" should be evaluated when call "(start-prime-test n (current-inexact-milliseconds))",
;so the times should be unique and consistent.

(show-howmany-prime 100 3)
(show-howmany-prime 1000 3)
(show-howmany-prime 10000 3)

;101 *** 0.001953125
;103 *** 0.001953125
;107 *** 0.001953125
;1009 *** 0.0068359375
;1013 *** 0.0048828125
;1019 *** 0.0048828125
;10007 *** 0.013916015625
;10009 *** 0.013916015625
;10037 *** 0.01416015625
