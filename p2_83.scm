#lang racket

(define *table* (make-hash))
(define (get op type) (hash-ref *table* (list op type)))
(define (put op type val) (hash-set! *table* (list op type) val))

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

;;;

(define (install-integer-package)
  (define (tag x) (attach-tag 'integer x))
  (put 'make 'integer (lambda (x) (tag x)))
  (define (integer->rational x) (make-rational x 1))
  (put 'raise '(integer) integer->rational)
  'done)

(define (install-rational-package)
  (define (tag x) (attach-tag 'rational x))
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
  (define (rational->real x) (make-real (/ (numer x) (denom x))))
  (put 'raise '(rational) rational->real)
  'done)

(define (install-real-package)
  (define (tag x) (attach-tag 'real x))    
  (put 'make 'real (lambda (x) (tag x)))
  (define (real->complex x) (make-complex x 0))
  (put 'raise '(real) real->complex)
  'done)

(define (install-complex-package)
  (define (tag x) (attach-tag 'complex x))
  (define (make-complex x y) (cons x y))
  (put 'make 'complex (lambda (x y) (tag (make-complex x y))))
  'done)

(install-integer-package)
(install-rational-package)
(install-real-package)
(install-complex-package)

(define (make-integer x) ((get 'make 'integer) x))
(define (make-rational n d) ((get 'make 'rational) n d))
(define (make-real x) ((get 'make 'real) x))
(define (make-complex x y) ((get 'make 'complex) x y))

(define (raise x) (apply-generic 'raise x))

;;;

(raise (make-integer 2))
(raise (make-rational 3 4))
(raise (make-real 1.23))