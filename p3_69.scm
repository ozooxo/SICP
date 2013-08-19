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

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (stream-cons (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (stream-cons
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (triples r s t)
  (let ([s-pair (pairs s t)])
    (stream-cons
     (cons (stream-car r) (stream-car s-pair))
     (interleave
      (stream-map (lambda (x) (cons (stream-car r) x))
                  (stream-cdr s-pair))
      (triples (stream-cdr r) (stream-cdr s) (stream-cdr t))))))

(stream-take 10 (triples integers integers integers))
;'((1 1 1) (1 1 2) (2 2 2) (1 2 2) (2 2 3) (1 1 3) (3 3 3) (1 2 3) (2 3 3) (1 1 4))

(define (square x) (* x x))
(stream-take 5 (stream-filter
                 (lambda (x) (= (+ (square (car x)) (square (cadr x))) (square (caddr x))))
                   (triples integers integers integers)))
;'((3 4 5) (6 8 10) (5 12 13) (9 12 15) (8 15 17))
;It runs really slowly. Thus I can't use it to check whether it goes all the Pythagorean triples or not.