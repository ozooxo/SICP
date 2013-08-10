#lang racket

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (list-of-vects->list-of-segments los)
  (let ([head (car los)])
    (define (list-of-vects->list-of-segments-iter los)
      (if (null? (cdr los))
          (list (make-segment (car los) head))
          (cons (make-segment (car los) (cadr los)) (list-of-vects->list-of-segments-iter (cdr los)))))
    (list-of-vects->list-of-segments-iter los)))

;(list-of-vects->list-of-segments (list (make-vect 0.2 0.3) (make-vect 0.4 0.5) (make-vect 0.6 0.7)))

;(a)

(define (frame-painter frame)
  (segments->painter
   (list-of-vects->list-of-segments (list ((frame-coord-map frame) (make-vect 0 0))
                                          ((frame-coord-map frame) (make-vect 0 1))
                                          ((frame-coord-map frame) (make-vect 1 1))
                                          ((frame-coord-map frame) (make-vect 1 0))))))

(define origin (make-vect 0.2 0.3))
(define edge1 (make-vect 0.1 0.5))
(define edge2 (make-vect 0.6 0.2))
(define frame (make-frame origin edge1 edge2))
(paint (frame-painter frame))

;(b)

(paint (segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 1)) (make-segment (make-vect 0 1) (make-vect 1 0)))))

;(c)

(define (diamond-painter frame)
  (segments->painter
   (list-of-vects->list-of-segments (list ((frame-coord-map frame) (make-vect 0 0.5))
                                          ((frame-coord-map frame) (make-vect 0.5 1))
                                          ((frame-coord-map frame) (make-vect 1 0.5))
                                          ((frame-coord-map frame) (make-vect 0.5 0))))))

(paint (diamond-painter frame))