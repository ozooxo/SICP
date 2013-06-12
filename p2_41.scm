#lang racket

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

(define (unique-triple n)
  (flatmap
   (lambda (i) (flatmap
                (lambda (j) (map (lambda (k) (list i j k)) (enumerate-interval 1 (- j 1))))
                (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

;(unique-triple 5) ;'((3 2 1) (4 2 1) (4 3 1) (4 3 2) (5 2 1) (5 3 1) (5 3 2) (5 4 1) (5 4 2) (5 4 3))

(define (sum-triple n s)
  (define (sum-n? triple)
    (= (+ (car triple) (car (cdr triple)) (car (cdr (cdr triple)))) s))
  (filter sum-n? (unique-triple n))) ;"filter" is defined in Racket.

(sum-triple 5 8) ;'((4 3 1) (5 2 1))