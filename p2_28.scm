#lang racket

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (fringe list)
  (cond ((and (null? (cdr list)) (number? (car list)) list))
        ((null? (cdr list)) (fringe (car list)))
        ((number? (car list)) (cons (car list) (fringe (cdr list))))
        (else (append (fringe (car list)) (fringe (cdr list))))))

(define x (list (list 1 2) (list 3 4)))

(fringe x) ;'(1 2 3 4)
(fringe (list x x)) ;'(1 2 3 4 1 2 3 4)