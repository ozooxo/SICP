#lang racket

(require racket/stream)

(define the-empty-stream empty-stream)
(define stream-null? stream-empty?)
(define stream-car stream-first)
(define stream-cdr stream-rest)

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

(define (integral integrand initial-value dt)
  (define int
    (stream-cons initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (stream-take n s)
  (if (= n 0)
      '()
      (cons (stream-car s) (stream-take (- n 1) (stream-cdr s)))))

;;;

;Notice that scheme cannot do currying, so we cannot do (RC R C dt s) ...
(define (RC R C dt)
  (lambda (i v0)
    (stream-map
     (lambda (x) (+ x (* R (stream-car i))))
     (add-streams (scale-stream (integral i 0 dt) (/ 1 C)) v0))))

(define RC1 (RC 5 1 0.5))

(define ones (stream-cons 1 ones))
(stream-take 5 (RC1 ones ones)) ;'(6 6.5 7.0 7.5 8.0)

;This problem seems actually quite far away from a real circuit case, so it is really
;quite hard to show a stream of result which makes sense:-(