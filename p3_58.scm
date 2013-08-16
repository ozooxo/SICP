#lang racket

(require racket/stream)

(define the-empty-stream empty-stream)
(define stream-null? stream-empty?)
(define stream-car stream-first)
(define stream-cdr stream-rest)

(define (stream-take n s)
  (if (= n 0)
      '()
      (cons (stream-car s) (stream-take (- n 1) (stream-cdr s)))))

;;;

(define (expand num den radix)
  (stream-cons
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(stream-take 10 (expand 1 7 10))
;== '(1*10/7 (expand 1*10%7 7 10))
;== '(1*10/7 (1*10%7)*10/7 (expand (1*10%7)*10/7 7 10))
;It is like doing 1/7 in Decimal numeral system.
;'(1 4 2 8 5 7 1 4 2 8)
(exact->inexact (/ 1 7)) ;0.14285714285714286

(stream-take 10 (expand 3 8 10)) ;'(3 7 5 0 0 0 0 0 0 0)
(exact->inexact (/ 3 8)) ;0.375