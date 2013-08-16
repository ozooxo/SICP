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

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (add-streams s1 s2)
  (stream-map-multiple-arguments + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

;;;

;Exercise 3.53

(define (stream-take n s)
  (if (= n 0)
      '()
      (cons (stream-car s) (stream-take (- n 1) (stream-cdr s)))))

(define s (stream-cons 1 (add-streams s s))) ;'(1,2,4,8,16...)
(stream-take 5 s) ;'(1 2 4 8 16)

;;;

;Exercise 3.54

(define (mul-streams s1 s2)
  (stream-map-multiple-arguments * s1 s2))

(define factorials (stream-cons 1 (mul-streams factorials (stream-map (lambda (x) (+ x 1)) integers))))

(stream-take 10 factorials) ;'(1 2 6 24 120 720 5040 40320 362880 3628800)

;;;

;Exercise 3.55

(define (partial-sums s) (stream-cons (stream-car s) (add-streams (stream-cdr s) (partial-sums s))))

(stream-take 5 (partial-sums integers)) ;'(1 3 6 10 15)

;;;

;Exercise 3.56

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (stream-cons s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (stream-cons s2car (merge s1 (stream-cdr s2))))
                 (else
                  (stream-cons s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define s235 (stream-cons 1 (merge (scale-stream s235 2) (merge (scale-stream s235 3) (scale-stream s235 5)))))

(stream-take 20 s235) ;'(1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36)