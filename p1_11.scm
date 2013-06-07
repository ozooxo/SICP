#lang racket

(define (f1 n)
  (cond ((< n 3) n)
        (else (+ (f1 (- n 1)) (* 2 (f1 (- n 2))) (f1 (- n 3))))))

(f1 6)

(define (f2 n)
  (f2-iter 2 1 0 n))

(define (f2-iter k l m count)
  (if (= count 0)
      m
      (f2-iter (+ k (* 2 l) m) k l (- count 1))))

(f2 6)