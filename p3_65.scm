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

(define (partial-sums s) (stream-cons (stream-car s) (add-streams (stream-cdr s) (partial-sums s))))

(define (stream-take n s)
  (if (= n 0)
      '()
      (cons (stream-car s) (stream-take (- n 1) (stream-cdr s)))))

;;;

(define (square x) (* x x))

(define (log2-summands n)
  (stream-cons (/ 1.0 n)
               (stream-map - (log2-summands (+ n 1)))))
(define log2-stream
  (partial-sums (log2-summands 1)))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (stream-cons (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (stream-cons s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(stream-take 7 log2-stream)
(stream-take 7 (euler-transform log2-stream))
(stream-take 7 (accelerated-sequence euler-transform log2-stream))
;The three series converge better and better.

(log 2) ;0.6931471805599453