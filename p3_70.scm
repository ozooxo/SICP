#lang racket

(require racket/stream)

(define the-empty-stream empty-stream)
(define stream-null? stream-empty?)
(define stream-car stream-first)
(define stream-cdr stream-rest)

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (stream-take n s)
  (if (= n 0)
      '()
      (cons (stream-car s) (stream-take (- n 1) (stream-cdr s)))))

;;;

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ([s1car (stream-car s1)]
               [s2car (stream-car s2)])
           (cond ((<= (weight s1car) (weight s2car))
                  (stream-cons s1car (merge-weighted (stream-cdr s1) s2 weight)))
                 (else
                  (stream-cons s2car (merge-weighted s1 (stream-cdr s2) weight))))))))

(define (pairs s t weight)
  (stream-cons
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

;Exercise 3.70a
(stream-take 15 (pairs integers integers (lambda (x) (+ (car x) (cadr x)))))
;'((1 1) (1 2) (1 3) (2 2) (1 4) (2 3) (1 5) (2 4) (3 3) (1 6) (2 5) (3 4) (1 7) (2 6) (3 5))

;Exercise 3.70b
(define (not-devided-by-235 x)
  (and (> (remainder x 2) 0)
       (> (remainder x 3) 0)
       (> (remainder x 5) 0)))

(stream-take 15 (stream-filter
                 (lambda (x) (and (not-devided-by-235 (car x)) (not-devided-by-235 (cadr x))))
                 (pairs
                  integers
                  integers
                  (lambda (x) (+ (+ (* 2 (car x)) (* 3 (cadr x))) (* 5 (* (car x) (cadr x))))))))
;'((1 1) (1 7) (1 11) (1 13) (1 17) (1 19) (1 23) (1 29) (1 31) (7 7) (1 37) (1 41) (1 43) (1 47) (1 49))