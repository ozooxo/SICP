#lang racket

(define (g)
  (let ([const 1])
    (define (h x)
      (begin (set! const (* const x))
             (* const x)))
    h))

(define f (g))

;(f 0) ;0
;(f 1) ;0

;(f 1) ;1
;(f 0) ;0

(+ (f 0) (f 1)) ;0
;So "+" is evaluated from left to right.