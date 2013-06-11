#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map proc tree)
  (cond ((null? (cdr tree)) (cons (proc (car tree)) '()))
        (else (cons (proc (car tree)) 
                    (map proc (cdr tree))))))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

;;;

(define (count-leaves t)
  (accumulate (lambda (x y) (+ y 1)) 0 (enumerate-tree t))) ;Works. But it doesn't use the form "(map <??> <??>)"
                                                            ;for the third argument of "accumulate". Is there a better way?

(define x (cons (list 1 2) (list 3 4)))
(count-leaves (list x x)) ;8
