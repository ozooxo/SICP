#lang racket

(define *table* (make-hash))
(define (get op type) (hash-ref *table* (list op type)))
(define (put op type val) (hash-set! *table* (list op type) val))

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

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
  (put 'equ? '(integer integer)
       (lambda (x y) (= x y)))
  (put '=zero? '(integer)
       (lambda (x) (= x 0)))
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
  (put 'equ? '(rational rational)
       (lambda (x y) (and (= (numer x) (numer y)) (= (denom x) (denom y)))))
  (put '=zero? '(rational)
       (lambda (x) (= (numer x) 0)))
  (define (drop x) (if (= (denom x) 1)
                       (make-integer (numer x))
                       (tag x)))
  (put 'project '(rational) drop)
  (define (rational->real x) (make-real (/ (numer x) (denom x))))
  (put 'raise '(rational) rational->real)
  'done)

(define (install-real-package)
  (define (tag x) (attach-tag 'real x))
  (put 'make 'real (lambda (x) (tag x)))
  (put 'equ? '(real real)
       (lambda (x y) (= x y)))
  (put '=zero? '(real)
       (lambda (x) (= x 0)))
  (define (drop x) (if (integer? x)
                       (make-integer x)
                       (tag x))) ;It currenly can only go real->integer,
                                 ;it cannot go real->rational.
  (put 'project '(real) drop)
  (define (real->complex x) (make-complex x 0))
  (put 'raise '(real) real->complex)
  'done)

(define (install-complex-package)
  (define (square x) (* x x))
  ;;;
  (define (tag x) (attach-tag 'complex x))
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (make-complex x y) (cons x y))
  (put 'make 'complex (lambda (x y) (tag (make-complex x y))))
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (and (= (real-part z1) (real-part z2)) (= (imag-part z1) (imag-part z2)))))
  (put '=zero? '(complex)
       (lambda (z1) (= (magnitude z1) 0)))
  (define (drop z) (if (= (imag-part z) 0)
                       (make-real (real-part z))
                       (tag z)))
  (put 'project '(complex) drop)
  'done)

(install-integer-package)
(install-rational-package)
(install-real-package)
(install-complex-package)

(define (make-integer x) ((get 'make 'integer) x))
(define (make-rational n d) ((get 'make 'rational) n d))
(define (make-real x) ((get 'make 'real) x))
(define (make-complex x y) ((get 'make 'complex) x y))

(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

(define (raise x) (apply-generic 'raise x))
(define (project x) (apply-generic 'project x))

;My question for this problem:
;Is "project" and "drop" are just two names of the same thing?
;It seems that in usual case, we just define them as the same name.
;One as the function name in a package, one as the tag/function name of a generic operation.
(project (raise (make-integer 3))) ;'(integer . 3)
(project (make-real 2.345)) ;'(real . 2.345)
(project (make-real 2)) ;'(integer . 2)
(project (make-complex 2 1)) ;'(complex 2 . 1)
(project (make-complex 2 0)) ;'(real . 2)