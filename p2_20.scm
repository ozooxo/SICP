#lang racket

(define (same-parity x . y)
  (let ([parity (remainder x 2)])
    (define (fixed-parity list)
      (cond ((null? list) '())
            ((= (remainder (car list) 2) parity) (cons (car list) (fixed-parity (cdr list))))
            (else (fixed-parity (cdr list)))))
    (cons x (fixed-parity y))))

(same-parity 1 2 3 4 5 6 7) ;'(1 3 5 7)

(same-parity 2 3 4 5 6 7) ;'(2 4 6)