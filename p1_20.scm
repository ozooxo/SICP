#lang racket

(define (remainder-count-applicative-order a b count)
  (if (= b 0)
      count
      (remainder-count-applicative-order b (remainder a b) (+ count 1))))

(remainder-count-applicative-order 206 40 0) ;4

;;;

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (remainder-count-normal-order a b count)
  (new-if (= b 0)
      count
      (remainder-count-normal-order b (remainder a b) (+ count 1))))

;(remainder-count-normal-order 206 40 0) 
;ERROR MESSAGE "remainder: undefined for 0"
;BECAUSE BOTH "then-clause" and "else-clause" are evaluated in "new-if", no matter whether "then-clause" is true or not.
;So should the answer be INFINITY?