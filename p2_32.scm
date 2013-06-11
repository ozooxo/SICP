#lang racket

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (map proc tree)
  (cond ((null? (cdr tree)) (cons (proc (car tree)) '()))
        (else (cons (proc (car tree)) 
                    (map proc (cdr tree))))))
; We can only use "(map )" in Exercise 2.30, rather than "(tree-map )" in Exercise 2.31, 
; because "'() != (proc '()) = (append '(1) '()) = '(1) != '()".

(define (subsets s)
  (define (append-car rest)
    (append (list (car s)) rest))
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map append-car rest)))))

(subsets (list 1 2 3)) ;'(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))