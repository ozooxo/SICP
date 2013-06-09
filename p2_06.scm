#lang racket

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one
  (lambda (f) (lambda (x) (f x)))) ;Because zero projects everything to id, so "(n f) = (zero f) = id = (lambda (x) x)", and "((zero f) x) = 0".

(define two
  (lambda (f) (lambda (x) (f (f x))))) ;Because one project f to "x->f(x)", so "((n f) x) = (f x)".

(define (add m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))

;;;

(define f cos) ;Randomly chosing something non-trivial to do the test.

((zero f) 2) ;2

(((add-1 zero) f) 2) ;-0.4161468365471424
((one f) 2) ;-0.4161468365471424 consistent

(((add-1 (add-1 zero)) f) 2) ;0.9146533258523714
((two f) 2) ;0.9146533258523714 consistent

(((add two two) f) 2) ;0.8196106080000903
(((add-1 (add-1 (add-1 (add-1 zero)))) f) 2) ;0.8196106080000903 consistent