#lang racket

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (unique-pairs n)
  (flatmap
   (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? ;"filter" should be defined in Racket. It is defined in 
                          ;http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-4.html#%_toc_%_sec_Temp_181
                          ;in SICP.
               (unique-pairs n))))

(prime-sum-pairs 5) ;'((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7))