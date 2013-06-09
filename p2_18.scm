#lang racket

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (reverse list)
  (if (null? (cdr list))
      list
      (append (reverse (cdr list)) (cons (car list) '()))))

(reverse (list 1 4 9 16 25)) ;'(25 16 9 4 1)