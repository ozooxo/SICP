#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

;;;

(define (union-set set1 set2)
  (define (union-set-tmp set-got set1 set2)
    (cond ((null? set1) (append set-got set2))
          ((null? set2) (append set-got set1))
          (else (let ([x1 (car set1)]
                      [x2 (car set2)])
                   (cond ((> x1 x2) (union-set-tmp (append set-got (list x2)) set1 (cdr set2)))
                         ((< x1 x2) (union-set-tmp (append set-got (list x1)) (cdr set1) set2))
                         ((= x1 x2) (union-set-tmp (append set-got (list x2)) (cdr set1) (cdr set2))))))))
  (union-set-tmp '() set1 set2))

(union-set (list 1 2 3 5 7 8) (list 3 4 5 6 8 9)) ;'(1 2 3 4 5 6 7 8 9)