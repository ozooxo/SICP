#lang racket

;Using "reverse" rather than "append" to make the program more efficient.
;Currently, the program has a O(n) implementation.

(define (reverse torev-seq done-seq)
  (if (eq? torev-seq '())
      done-seq
      (reverse (cdr torev-seq) (cons (car torev-seq) done-seq))))

;;;

(define (union-set set1 set2)
  (define (union-set-tmp set-got set1 set2)
    (cond ((null? set1) (reverse set-got set2))
          ((null? set2) (reverse set-got set1))
          (else (let ([x1 (car set1)]
                      [x2 (car set2)])
                   (cond ((> x1 x2) (union-set-tmp (cons x2 set-got) set1 (cdr set2)))
                         ((< x1 x2) (union-set-tmp (cons x1 set-got) (cdr set1) set2))
                         ((= x1 x2) (union-set-tmp (cons x2 set-got) (cdr set1) (cdr set2))))))))
  (union-set-tmp '() set1 set2))

(union-set (list 1 2 3 5 7 8) (list 3 4 5 6 8 9)) ;'(1 2 3 4 5 6 7 8 9)