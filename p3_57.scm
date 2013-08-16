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

(define (stream-take n s)
  (if (= n 0)
      '()
      (cons (stream-car s) (stream-take (- n 1) (stream-cdr s)))))

;;;

(define fibs
  (stream-cons 0
               (stream-cons 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

(stream-take 10 fibs)
;For the nth Fibonacci number, we just need to calculate addition for (n-2) times.
;Because what is needed is already stored in the first (n-1) numbwer of the list "fibs".

;If we implemente (delay <exp>) simply as (lambda () <exp>), then we need more additions in the calculation.
;It goes like
;fibs 0 1 1+0 1+0+1 1+0+1+1+0 ...
;         1   1+0   1+0+1
;         0   1     1+0