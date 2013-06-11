#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (accumulate (lambda (x y) (cons (car x) y)) '() seqs))
            (accumulate-n op init (accumulate (lambda (x y) (cons (cdr x) y)) '() seqs)))))

;;;

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (define (dot-v w) (dot-product v w))
  (map dot-v m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector m x)) n))) ;If the definition is like "m.nT",
                                                 ;then we should use "(map <??> n)" rather than "(map <??> m)".
                                                 ;which is not exactly the same as what is given by the problem.

;;;

(define v (list 1 2 3 4))
(define w (list 5 6 7 8))
(define m (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12)))

(dot-product v w) ;70
(matrix-*-vector m v) ;'(30 70 110)
(transpose m) ;'((1 5 9) (2 6 10) (3 7 11) (4 8 12))
(matrix-*-matrix m m) ;'((30 70 110) (70 174 278) (110 278 446))