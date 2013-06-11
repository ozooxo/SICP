#lang racket

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (fringe list1)
  (cond ((null? list1) '())
        ((not (pair? list1)) (list list1))
        (else (append (fringe (car list1)) (fringe (cdr list1))))))

(define x (list (list 1 2) (list 3 4)))

(fringe x) ;'(1 2 3 4)
(fringe (list x x)) ;'(1 2 3 4 1 2 3 4)