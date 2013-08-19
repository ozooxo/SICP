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

(stream-take 15 (pairs integers integers))
;'((1 1) (1 2) (2 2) (1 3) (2 3) (1 4) (3 3) (1 5) (2 4) (1 6) (3 4) (1 7) (2 5) (1 8) (4 4))

;For the order, the numbers which already go for the first line, equal or more than the numbers from the rest lines.
;The numbers of the second line, equal or more than the numbers from 3+4+5+6... lines. Etc, etc.

(define (stream-find s s0)
  (define (stream-find-iter s n)
    (if (equal? (stream-car s) s0)
        n
        (stream-find-iter (stream-cdr s) (+ n 1))))
  (stream-find-iter s 0))

;So for (1,100), it has the index 99*2-1 = 197 (the first one has index 0).
(stream-find (pairs integers integers) '(1 100)) ;197

;For (99,100), it is the ((2*2+1)*2+1)*2+1...)-1 doing *2+1 for 98 times pair.
(define (count-iter x) (stream-cons x (count-iter (+ (* x 2) 1))))
(define count-x-xpp (count-iter 2))
(- (stream-ref count-x-xpp 98) 1) ;950737950171172051122527404030

;Double check for something smaller, e.g. (10,11).
(- (stream-ref count-x-xpp 9) 1) ;1534
(stream-find (pairs integers integers) '(10 11)); 1534

;For (100,100)
(define count-x-x (count-iter 1))
(- (stream-ref count-x-x 99) 1) ;1267650600228229401496703205374

;Double check for something smaller, e.g. (11,11).
(- (stream-ref count-x-x 10) 1) ;2046
(stream-find (pairs integers integers) '(11 11)); 2046