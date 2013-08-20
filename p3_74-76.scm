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

(define (stream-take n s)
  (if (= n 0)
      '()
      (cons (stream-car s) (stream-take (- n 1) (stream-cdr s)))))

;;;

(define (sign-change-detector x y)
  (cond ((and (>= x 0) (< y 0)) -1)
        ((and (< x 0) (>= y 0)) 1)
        (else 0)))

(define (make-zero-crossings input-stream last-value)
  (stream-cons
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))

;(define zero-crossings (make-zero-crossings sense-data 0))

;;;

;Exercise 3.74

;Use method of Alyssa P. Hacker.

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define sins (stream-map sin integers)) ;This is the false sense-data I make.

;(stream-take 10 sins)
(stream-take 20 (make-zero-crossings sins 0)) ;okay

;;;

;Use method of Eva Lu Ator.

(stream-take
 20
 (stream-map-multiple-arguments sign-change-detector sins (stream-cons 0 sins))) ;same as above

;;;

;Exercise 3.75

;The way Louis Reasoner's code doesn't work, is that last-value already averages the last and 
;the one before the last values. So, when we do "(avpt (/ (+ (stream-car input-stream) last-value) 2))"
;we actually average with weight half of this value, 1/4 of last value, 1/8 of the one before the last
;value, and so on. So it does not do what Alyssa want.

(define (make-zero-crossings-new input-stream real-last-value avpt-last-value)
  (let ((avpt (/ (+ (stream-car input-stream) real-last-value) 2)))
    (stream-cons (sign-change-detector avpt avpt-last-value)
                 (make-zero-crossings-new (stream-cdr input-stream)
                                          (stream-car input-stream)
                                          avpt))))

(stream-take 20 (make-zero-crossings-new sins 0 0)) ;Not exactly the same as the former two streams.

;;;

;Exercise 3.76

(define (smooth s)
  (stream-cons (/ (+ (stream-car s) (stream-car (stream-cdr s))) 2)
               (smooth (stream-cdr s))))

(stream-take 20 (make-zero-crossings (smooth sins) 0)) ;It is the same as last line, but with a -1 shift.
                                                       ;As a stream, the shift can be neglected.
                                                       ;So I don't farther fine tune my code.