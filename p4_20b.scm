#lang racket

;Here is a way to show "let" and "letrec" are not the same, and "define" is transform
;to "letrec" in the sense of Exercise 4.16.

(let ([x 5])
  (let ([x 2]
        [y x])
    (list y x)))

;'(5 2)

(let ([x 5])
  (letrec ([x 2]
           [y x])
    (list y x)))

;'(2 2)

(let ([x 5])
  (define x 2)
  (define y x)
  (list y x))

;'(2 2)