#lang racket

(define (make-interval a b) (cons a b))
(define (upper-bound x) (max (car x) (cdr x)))
(define (lower-bound x) (min (car x) (cdr x)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y)))) ;not upper-lower and lower-upper based on
                                                       ;ex 2.9 "Show that the width of the sum (or difference) of two intervals is a
                                                       ;function only of the widths of the intervals being added (or subtracted)"

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (and (> (upper-bound y) 0) (< (lower-bound y) 0))
      (error "divide by an interval that spans zero")
      (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))

(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2.0))

;;;

(define x (make-interval 7 9))
(define y (make-interval 4 5))

(upper-bound (sub-interval x y))
(upper-bound (mul-interval x y))
(lower-bound (div-interval x y))

(newline)

(width (add-interval x y))
(+ (width x) (width y))

(width (sub-interval x y))
(- (width x) (width y))

(width (mul-interval x y))
(* (width x) (width y))