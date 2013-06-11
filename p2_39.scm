#lang racket

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;;;

(define (reverse-fold-right sequence)
  (fold-right (lambda (x y) (fold-right cons (list x) y)) '() sequence))

(define (reverse-fold-left sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

(define s (list 1 2 3 4))
(reverse-fold-right s) ;'(4 3 2 1)
(reverse-fold-left s) ;'(4 3 2 1)