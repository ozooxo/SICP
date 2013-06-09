#lang racket

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (deep-reverse list)
  (cond ((number? list) list)
        ((null? (cdr list)) (cons (deep-reverse (car list)) '()))
        (else (append (deep-reverse (cdr list)) (cons (deep-reverse (car list)) '())))))

(define x (list (list 1 2) (list 3 4)))
(deep-reverse x) ;'((4 3) (2 1))

(define y (list (list 1 2 3) (list 4 5 6) (list 7 8)))
(deep-reverse y) ;'((8 7) (6 5 4) (3 2 1))