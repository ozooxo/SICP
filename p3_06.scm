#lang racket

(define (rand-update x)
  (begin
    (random-seed x)
    (random 10)))

(define random-init 1)

(define rand
  (let ([x random-init])
    (define (generate)
      (set! x (rand-update x))
      x)
    (define (reset init)
      (set! x init))
    (define (dispatch m)
      (cond ((eq? m 'generate) (generate))
            ((eq? m 'reset) reset)
            (else (error "Unknown request -- RAND" m))))
    dispatch))

(rand 'generate) ;5
((rand 'reset) 1)
(rand 'generate) ;5 ;the same as before
(rand 'generate) ;3