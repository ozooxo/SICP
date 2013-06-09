#lang racket

(define (make-point x y)
  (cons x y))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment line) (car line))

(define (end-segment line) (cdr line))

(define (midpoint-segment line)
  (make-point (/ (+ (x-point (start-segment line)) (x-point (end-segment line))) 2.0)
              (/ (+ (y-point (start-segment line)) (y-point (end-segment line))) 2.0)))

(define p1 (make-point 0 0))
(define p2 (make-point 1 0))
(define p3 (make-point 1 2))
(define p4 (make-point 0 2))

;;;

;(define (make-rectangle p1 p3)
;  (cons p1 p3))
;
;(define (horizontal-edge rectangle)
;  (abs (- (x-point (car rectangle)) (x-point (cdr rectangle)))))
;
;(define (vertical-edge rectangle)
;  (abs (- (y-point (car rectangle)) (y-point (cdr rectangle)))))
;
;(define rectangle (make-rectangle p1 p3))

;;;

(define (make-rectangle l1 l2)
  (define (same-point p1 p2)
    (and (= (x-point p1) (x-point p2)) (= (y-point p1) (y-point p2))))
  (if (same-point (start-segment l1) (start-segment l2))
      (cons l1 l2)
      (error "start point not the same")))
      ; There are several other things I should check, such as whether l1 is horizontal and whether l2 is vertical. I'm tired to do so.

(define (horizontal-edge rectangle)
  (abs (- (x-point (start-segment (car rectangle))) (x-point (end-segment (car rectangle))))))

(define (vertical-edge rectangle)
  (abs (- (y-point (start-segment (cdr rectangle))) (y-point (end-segment (cdr rectangle))))))

(define l1 (make-segment p1 p2))
(define l2 (make-segment p1 p4))
(define rectangle (make-rectangle l1 l2))

;;;

(define (perimeter rectangle)
  (* 2 (+ (horizontal-edge rectangle) (vertical-edge rectangle))))

(define (area rectangle)
  (* (horizontal-edge rectangle) (vertical-edge rectangle)))

(perimeter rectangle) ;6
(area rectangle) ;2