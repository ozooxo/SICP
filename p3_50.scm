#lang racket

(require racket/stream)

(define the-empty-stream empty-stream)
(define stream-null? stream-empty?)
;(define cons-stream stream-cons) ;I don't understand why the redifinition doesn't work.
(define stream-car stream-first)
(define stream-cdr stream-rest)

;;;

(define (stream-map-multiple-arguments proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car argstreams))
       (apply stream-map-multiple-arguments
              (cons proc (map stream-cdr argstreams))))))

;;;

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (stream-cons
       low
       (stream-enumerate-interval (+ low 1) high))))

;(define (integers-starting-from n)
;  (stream-cons n (integers-starting-from (+ n 1))))

(define test-stream (stream-map-multiple-arguments + (stream-enumerate-interval 0 9) (stream-enumerate-interval 1 10)))
(stream-car test-stream) ;3
(stream-car (stream-cdr test-stream)) ;5
(stream->list test-stream) ;'{1 3 5 7 9 11 13 15 17 19}