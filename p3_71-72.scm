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

;Exercise 3.71

(define (cube x) (* x x x))
(define (cube-sum p) (+ (cube (car p)) (cube (cadr p))))
(define pairs-i3j3 (pairs integers integers cube-sum))

(define (ramanujan-numbers-from s)
  (let ([cube-sum1 (cube-sum (stream-car s))]
        [cube-sum2 (cube-sum (stream-car (stream-cdr s)))])
    (if (= cube-sum1 cube-sum2)
        (stream-cons cube-sum1 (ramanujan-numbers-from (stream-cdr s)))
        (ramanujan-numbers-from (stream-cdr s)))))

(define ramanujan-numbers
  (ramanujan-numbers-from pairs-i3j3))

(stream-take 5 ramanujan-numbers) ;'(1729 4104 13832 20683 32832)

;Exercise 3.72

(define (square x) (* x x))
(define (square-sum p) (+ (square (car p)) (square (cadr p))))
(define pairs-i2j2 (pairs integers integers square-sum))

(define (three-square-sum-from s)
  (let ([square-sum1 (square-sum (stream-car s))]
        [square-sum2 (square-sum (stream-car (stream-cdr s)))]
        [square-sum3 (square-sum (stream-car (stream-cdr (stream-cdr s))))])
    (if (and (= square-sum1 square-sum2) (= square-sum1 square-sum3))
        (stream-cons square-sum1 (three-square-sum-from (stream-cdr s)))
        (three-square-sum-from (stream-cdr s)))))

(define three-square-sum
  (three-square-sum-from pairs-i2j2))

(stream-take 5 three-square-sum) ;'(325 425 650 725 845)