#lang racket

;Using "reverse" rather than "append" to make the program more efficient.

(define (reverse torev-seq done-seq)
  (if (eq? torev-seq '())
      done-seq
      (reverse (cdr torev-seq) (cons (car torev-seq) done-seq))))

;(reverse (list 2 1) (list 3 4 5)) ;'(1 2 3 4 5)

;;;

(define (adjoin-set x set)
  (define (adjoin-set-tmp x set-before set-after)
    (cond ((eq? set-after '()) (reverse set-before (list x)))
          ((< x (car set-after)) (reverse set-before (cons x set-after)))
          ((= x (car set-after)) (reverse set-before set-after))
          ((> x (car set-after)) (adjoin-set-tmp x (cons (car set-after) set-before) (cdr set-after)))))
  (adjoin-set-tmp x '() set))

(adjoin-set 1 (list 2 3 4 5)) ;'(1 2 3 4 5)
(adjoin-set 3 (list 1 2 4 5)) ;'(1 2 3 4 5)
(adjoin-set 3 (list 1 2 3 5)) ;'(1 2 3 5)
(adjoin-set 8 (list 1 2 3 5)) ;'(1 2 3 5 8)