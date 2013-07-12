#lang racket

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))

;==
;(define (f x)
;  ((lambda (ev? od? n)
;     (if (= n 0) true (od? ev? od? (- n 1))))
;   (lambda (ev? od? n)
;     (if (= n 0) true (od? ev? od? (- n 1))))
;   (lambda (ev? od? n)
;     (if (= n 0) false (ev? ev? od? (- n 1))))
;   x))
;==

(f 0)
(f 1)
(f 2)
(f 3)
(f 4)
(f 5)
(f 6)