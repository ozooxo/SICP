#lang racket

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define z1 (list 'a 'b 'c)) 
(count-pairs z1) ;3

(define x (list 'a 'b))
(define z2 (cons x (cdr x)))
(count-pairs z2) ;4

(define y (list 'a))
(define w (cons y y))
(define z3 (cons w w))
(count-pairs z3) ;7

(define (count-mpairs x)
  (if (not (mpair? x))
      0
      (+ (count-mpairs (mcar x))
         (count-mpairs (mcdr x))
         1)))

(define (make-cycle x)
  (set-mcdr! (last-pair x) x)
  x)
(define (last-pair x)
  (if (null? (mcdr x))
      x
      (last-pair (mcdr x))))
(define z4 (make-cycle (mcons 'a (mcons 'b (mcons 'c '())))))

(count-mpairs z4) ;never return at all