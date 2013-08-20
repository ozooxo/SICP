#lang racket

(require racket/stream)

(define the-empty-stream empty-stream)
(define stream-null? stream-empty?)
(define stream-car stream-first)
(define stream-cdr stream-rest)

(define (stream-take n s)
  (if (= n 0)
      '()
      (cons (stream-car s) (stream-take (- n 1) (stream-cdr s)))))

;;;

(define (float->int x) (inexact->exact (truncate (* x 2147483647))))

(define (rand-update x)
  (begin
    (random-seed (float->int x))
    (random)))

(define random-init (random))

(define random-numbers 
  (stream-cons random-init
               (stream-map rand-update random-numbers)))

(define (random-in-range low high)
  (stream-map (lambda (x) (+ low (* (random) (- high low)))) random-numbers))

;(stream-take 5 (random-in-range 2 6))

(define (P-stream x1 x2 y1 y2)
  (define (square x) (* x x))
  (define x (random-in-range x1 x2))
  (define y (random-in-range y1 y2))
  (define (p x y)
    (stream-cons
     (not (> (+ (square (- (stream-car x) 5)) (square (- (stream-car y) 7))) (square 3)))
     (p (stream-cdr x) (stream-cdr y))))
  (p x y))

(stream-take 20 (P-stream 2 8 4 10))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (stream-cons
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (rational->real x) (* 1.0 x))
(rational->real (stream-ref (monte-carlo (P-stream 2 8 4 10) 0 0) 1000)) ;0.7892107892107892
(/ (* pi 3 3) (* 6 6)) ;0.7853981633974483
;consistent