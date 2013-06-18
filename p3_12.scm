#lang racket

(define (append x y)
  (if (null? x)
      y
      (mcons (mcar x) (append (mcdr x) y))))

(define (append! x y)
  (set-mcdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (mcdr x))
      x
      (last-pair (mcdr x))))

(define x (mcons 'a (mcons 'b '()))) ;Are there a better way to write a mutable list in Racket?
(define y (mcons 'c (mcons 'd '())))
(define z (append x y)) 
z ;(mcons 'a (mcons 'b (mcons 'c (mcons 'd '()))))
(mcdr x) ;(mcons 'b '())

(define w (append! x y))
w ;(mcons 'a (mcons 'b (mcons 'c (mcons 'd '()))))
(mcdr x) ;(mcons 'b (mcons 'c (mcons 'd '())))
