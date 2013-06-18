#lang racket

(define (in-list? element target-list)
  (cond ((null? target-list) false)
        ((eq? element (car target-list)) true)
        (else (in-list? element (cdr target-list)))))

(define (count-pairs x)
  (let ([counted-list '()])
    (define (count-pairs-tmp x)
      (cond ((not (pair? x)) 0)
            ((in-list? x counted-list) 0)
            (else (begin
                    (set! counted-list (cons x counted-list))
                    (+ (count-pairs-tmp (car x))
                    (count-pairs-tmp (cdr x))
                    1)))))
    (count-pairs-tmp x)))

(define z1 (list 'a 'b 'c)) 
(count-pairs z1) ;3

(define x (list 'a 'b))
(define z2 (cons x (cdr x)))
(count-pairs z2) ;3

(define y (list 'a))
(define w (cons y y))
(define z3 (cons w w))
(count-pairs z3) ;3

;I didn't do the loop one, because for that one, I need to change every "cons"
;to "mcons" in Racket, and the basic idea is around the same.
