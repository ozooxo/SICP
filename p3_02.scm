#lang racket

(define (make-monitored proc)
  (let ([count 0])
    (define (run-proc arg)
      (begin (set! count (+ count 1))
             (proc arg)))
    (define (how-many-calls?) count)
    (define (reset-count) (set! count 0))
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) (how-many-calls?))
            ((eq? m 'reset-count) (reset-count))
            (else (run-proc m))))
    dispatch))

(define s (make-monitored sqrt))
(s 100) ;10
(s 'how-many-calls?) ;1
(s 'reset-count)
(s 'how-many-calls?) ;0