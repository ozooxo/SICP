#lang racket

(require racket/stream)

(define the-empty-stream empty-stream)
(define stream-null? stream-empty?)
(define stream-car stream-first)
(define stream-cdr stream-rest)

(define (delay expression) (lambda () expression))
(define (force delayed-object) (delayed-object))

(define (stream-map-multiple-arguments proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car argstreams))
       (apply stream-map-multiple-arguments
              (cons proc (map stream-cdr argstreams))))))

(define (add-streams s1 s2)
  (stream-map-multiple-arguments + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (integral1 delayed-integrand initial-value dt)
  (define int
    (stream-cons initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

(define (solve f y0 dt)
  (define y (integral1 (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

;(stream-ref (solve (lambda (y) y) 1 0.001) 1000) ;error
;So even the SICP example doesn't run in Racket?

;;;

(define (integral2 delayed-integrand initial-value dt)
  (let ((integrand (force delayed-integrand)))
    (stream-cons initial-value
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral2 (stream-cdr delayed-integrand)
                                (+ (* dt (stream-car integrand))
                                   initial-value)
                                dt)))))

;No checking, as I cannot get the original SICP example run:-(