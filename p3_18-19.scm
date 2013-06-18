#lang racket

(define (in-list? element target-mlist)
  (cond ((null? target-mlist) false)
        ((eq? element (mcar target-mlist)) true)
        (else (in-list? element (mcdr target-mlist)))))

(define (cycle? x)
  (define (cycle?-tmp x counted-list)
    (cond ((not (mpair? x)) false)
          ((in-list? x counted-list) true)
          (else (or (cycle?-tmp (mcar x) (mcons x counted-list))
                    (cycle?-tmp (mcdr x) (mcons x counted-list))))))
  (cycle?-tmp x '()))

(define z1 (mcons 'a (mcons 'b (mcons 'c '())))) 
(cycle? z1) ;#f

(define x (mcons 'b (mcons 'c '())))
(define z2 (mcons x (mcdr x)))
(cycle? z2) ;#f

(define y (mcons 'a '()))
(define w (mcons y y))
(define z3 (mcons w w))
(cycle? z3) ;#f

(define (make-cycle x)
  (set-mcdr! (last-pair x) x)
  x)
(define (last-pair x)
  (if (null? (mcdr x))
      x
      (last-pair (mcdr x))))
(define z4 (make-cycle (mcons 'a (mcons 'b (mcons 'c '())))))
(cycle? z4) ;#t

;It seems that this algorithm already just takes only a constant amount of space,
;but it is not smart. Is there a better way?