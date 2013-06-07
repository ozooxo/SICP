#lang racket

(define (pascal m n)
  (cond ((= m 0) 1)
        ((= m n) 1)
        ((< m 0) 0)
        ((> m n) 0)
        (else (+ (pascal m (- n 1)) (pascal (- m 1) (- n 1))))))

(pascal 0 5)
(pascal 1 5)
(pascal 2 5)
(pascal 3 5)
(pascal 4 5)
(pascal 5 5)