#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

;;;

(define (adjoin-set x set)
  (define (adjoin-set-tmp x set-before set-after)
    (cond ((eq? set-after '()) (append set-before (list x)))
          ((< x (car set-after)) (append set-before (cons x set-after)))
          ((= x (car set-after)) (append set-before set-after))
          ((> x (car set-after)) (adjoin-set-tmp x (append set-before (list (car set-after))) (cdr set-after)))))
  (adjoin-set-tmp x '() set))

(adjoin-set 1 (list 2 3 4 5)) ;'(1 2 3 4 5)
(adjoin-set 3 (list 1 2 4 5)) ;'(1 2 3 4 5)
(adjoin-set 3 (list 1 2 3 5)) ;'(1 2 3 5)
(adjoin-set 8 (list 1 2 3 5)) ;'(1 2 3 5 8)