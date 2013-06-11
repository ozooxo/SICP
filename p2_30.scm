#lang racket

(define (square x) (* x x))

;Define directly
;(define (square-tree tree)
;  (cond ((null? tree) '())
;        ((not (pair? tree)) (square tree))
;        (else (cons (square-tree (car tree))
;                    (square-tree (cdr tree))))))

;Using "map" and recursion
(define (map proc tree)
  (cond ((null? (cdr tree)) (cons (proc (car tree)) '()))
        ;Cannot do "((null? tree) '())" unless also check "pair?" for (car tree). Alternative:
            ;((null? tree) '())
            ;((not (pair? tree)) (proc tree))
        (else (cons (proc (car tree)) 
                    (map proc (cdr tree))))))
        ;"(car tree)" cannot be '(), but "(cdr tree)" can. So "(car tree)" need not be "(map proc ...)" but "(cdr tree)" need.
        ;"cond" actually depends on what we can or cannot handle in "(lambda ...)", such make the program extremely confusing.

        ;The reason to design this weird map can be seen in Exercise 2.32.
        ;It may be possible that "(car tree)='()" (e.g. in case "'(())").
        ;For some process like "(append '(1) '())", there's no reason that "(proc '())=='()", so Exercise 2.31 doesn't work.

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree) ;Unlike "(map square items)" which is linear, "lambda" recurs "square-tree".
             (square sub-tree)))
       tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7))) ;'(1 (4 (9 16) 25) (36 49))