#lang racket

(define (even? n)
  (= (remainder n 2) 0))

(define (expt-iter a b n)
  (if (= n 0)
      a
      (if (even? n)
          (expt-iter a (* b b) (/ n 2))
          (expt-iter (* a b) (* b b) (/ (- n 1) 2)))))

(define (expt x n)
  (expt-iter 1 x n))

(expt 2 10)