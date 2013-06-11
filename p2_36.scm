#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (accumulate-n op init seqs)
  (cond ((null? (car seqs))
         '())
        ((and (pair? (car seqs)) (null? (cdr seqs)))
         (cons (accumulate op init (list (car (car seqs))))
               (accumulate-n op init (list (cdr (car seqs))))))
        (else
         (cons (accumulate op init (list (car (car seqs)) (car (accumulate-n op init (cdr seqs)))))
               (accumulate-n op init (list (cdr (car seqs)) (cdr (accumulate-n op init (cdr seqs)))))))))
;I did't get it exactly in the partern of
;(define (accumulate-n op init seqs)
;  (if (null? (car seqs))
;      '()
;      (cons (accumulate op init <??>)
;            (accumulate-n op init <??>)))) ,
;because "accumulate-n" need to handle both the very bottom and the very right of the matrix.

(define (add-matrix s) (accumulate-n + 0 s))

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(add-matrix s) ;'(22 26 30)

