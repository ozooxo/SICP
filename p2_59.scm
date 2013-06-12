#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (union-set set1 set2)
  (cond ((eq? set1 '()) set2)
        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
        (else (union-set (cdr set1) (cons (car set1) set2)))))

(define s1 (list 1 2 3))
(define s2 (list 2 3 4))
(union-set s1 s2) ;'(1 2 3 4)