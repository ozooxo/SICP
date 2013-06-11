#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (accumulate (lambda (x y) (cons (car x) y)) '() seqs))
            (accumulate-n op init (accumulate (lambda (x y) (cons (cdr x) y)) '() seqs)))))

(define (add-matrix s) (accumulate-n + 0 s))

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(add-matrix s) ;'(22 26 30)

;The reason is:
;(accumulate (lambda (x y) (cons (car x) y)) '() s) ;'(1 4 7 10)
;(accumulate (lambda (x y) (cons (cdr x) y)) '() s) ;'((2 3) (5 6) (8 9) (11 12))