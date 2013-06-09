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
  (cond ((and (> (lower-bound x) 0) (> (lower-bound y) 0)) 
         (make-interval (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))
        ((and (< (upper-bound x) 0) (< (upper-bound y) 0)) 
         (make-interval (* (upper-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y))))
        ((and (> (lower-bound x) 0) (< (upper-bound y) 0)) 
         (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y))))
        ((and (< (upper-bound x) 0) (> (lower-bound y) 0)) 
         (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (upper-bound y))))
        ((and (> (lower-bound x) 0) (< (lower-bound y) 0) (> (upper-bound y) 0)) 
         (make-interval (* (upper-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))
        ((and (> (lower-bound y) 0) (< (lower-bound x) 0) (> (upper-bound x) 0)) 
         (make-interval (* (upper-bound y) (lower-bound x)) (* (upper-bound y) (upper-bound x))))
        ((and (< (upper-bound x) 0) (< (lower-bound y) 0) (> (upper-bound y) 0)) 
         (make-interval (* (lower-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y))))
        ((and (< (upper-bound y) 0) (< (lower-bound x) 0) (> (upper-bound x) 0)) 
         (make-interval (* (lower-bound y) (upper-bound x)) (* (lower-bound y) (lower-bound x))))
        ((and (< (lower-bound x) 0) (> (upper-bound x) 0) (< (lower-bound y) 0) (> (upper-bound y) 0)) 
         (make-interval (min (* (lower-bound x) (upper-bound y)) (* (lower-bound y) (upper-bound x)))
                        (max (* (lower-bound x) (lower-bound y)) (* (upper-bound y) (upper-bound x)))))))

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

(upper-bound (mul-interval x y))

(width (mul-interval x y))
(* (width x) (width y))