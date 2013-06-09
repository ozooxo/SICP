#lang racket

(define (square x) (* x x))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) ;find out the next (right) element in "things"
              (cons (square (car things))
                    answer)))) ;add a new element in the left
  (iter items '()))

(square-list (list 1 2 3 4)) ;'(16 9 4 1)

;;;

;(define (square-list items)
;  (define (iter things answer)
;    (if (null? things)
;        answer
;        (iter (cdr things)
;              (cons answer
;                    (square (car things))))))
;  (iter items nil)) ; It doesn't work, because "(cons answer (square (car things)))" is no longer a chain of pair as we want.
                     ; It has the structure of "(cons (cons x y) z)".