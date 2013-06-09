#lang racket

(define (last-pair list)
  (if (null? (cdr list))
      list
      (last-pair (cdr list)))) ; So it is possible to use list as an argumeent, although "list" is a primitive in scheme.

(last-pair (list 23 72 149 34))