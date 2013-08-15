#lang racket

(define *table* (make-hash))
(define (get op type) (hash-ref *table* (list op type) false))
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

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (neg x) (apply-generic 'neg x))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

(define (remainder x y) (apply-generic 'remainder x y))
(define (greatest-common-divisor x y) (apply-generic 'gcd x y))

;;;

(define (install-real-package)
  (define (tag x) (attach-tag 'real x))   
  (put 'add '(real real) (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real) (lambda (x y) (tag (- x y))))
  (put 'mul '(real real) (lambda (x y) (tag (* x y))))
  (put 'div '(real real) (lambda (x y) (tag (/ x y))))
  (put 'neg '(real) (lambda (x) (tag (- 0 x))))
  (put 'equ? '(real real) (lambda (x y) (= x y)))
  (put '=zero? '(real) (lambda (x) (= x 0)))
  (put 'make 'real (lambda (x) (tag x)))
  (put 'gcd '(real real) (lambda (x y) (tag (gcd x y))))
  'done)

(install-real-package)

(define (make-real x) ((get 'make 'real) x))

;;;

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ([g (greatest-common-divisor n d)])
      (cons (div n g) (div d g))))
  ;(cons n d))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  ;(put 'equ? '(rational rational)
  ;     (lambda (x y) (and (= (numer x) (numer y)) (= (denom x) (denom y)))))
  ;(put '=zero? '(rational)
  ;     (lambda (x) (= (numer x) 0)))
  ;Those parts doesn't work for the general case right now.
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(install-rational-package)

(define (make-rational n d)
  ((get 'make 'rational) n d))

;;;

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ([t1 (first-term L1)]
              [t2 (first-term L2)])
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ([new-c (div (coeff t1) (coeff t2))] ;In here, "div" actually can only do reals but not polynomials,
                                                        ;the "div-poly" is by definition cannot do polynomials
                                                        ;with polynomial coefficients.
                    [new-o (- (order t1) (order t2))])
                (let ([rest-of-result
                       (add-terms L1 (neg-terms (mul-term-by-all-terms (make-term new-o new-c) L2)))
                       ])
                  (list (adjoin-term (make-term new-o new-c) (car (div-terms rest-of-result L2)))
                        (cadr (div-terms rest-of-result L2)))
                  ))))))
  (define (remainder-terms L1 L2) (cadr (div-terms L1 L2)))
  (define (gcd-terms a b)
    (if (empty-termlist? b)
        a
        (gcd-terms b (remainder-terms a b))))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY" (list p1 p2))))
  (define (mul-poly p1 p2) 
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY" (list p1 p2))))
  (define (div-poly p1 p2) ;The definition here is not the same as Exercise 2.91. The reminder term is not included.
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                         (car (div-terms (term-list p1)
                                         (term-list p2))))
        (error "Polys not in same var -- DIV-POLY" (list p1 p2))))
  (define (remainder-poly p1 p2) 
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                         (cadr (div-terms (term-list p1)
                                         (term-list p2))))
        (error "Polys not in same var -- DIV-POLY" (list p1 p2))))
  (define (gcd-poly p1 p2) 
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (gcd-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY" (list p1 p2))))
  (define (neg-terms L)
    (if (empty-termlist? L)
        L
        (let ([fst (first-term L)])
          (adjoin-term (make-term (order fst) (neg (coeff fst))) (neg-terms (rest-terms L))))))
  (define (neg-poly p)
    (make-poly (variable p) (neg-terms (term-list p))))
  (define (=zero?-poly p) (null? (term-list p)))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial) (lambda (p1 p2) (tag (div-poly p1 p2))))
  (put 'remainder '(polynomial polynomial) (lambda (p1 p2) (tag (remainder-poly p1 p2))))
  (put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms))))
  (put 'neg '(polynomial) (lambda (x) (tag (neg-poly x))))
  (put 'sub '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 (neg-poly p2)))))
  (put '=zero? '(polynomial) =zero?-poly)
  (put 'gcd '(polynomial polynomial) (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  'done)

(install-polynomial-package)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

;;;

;Exercise 2.93
;(It seems we need to finish 2.94 to solve 2.93?)

(define p1 (make-polynomial 'x (list (list 2 (make-real 1)) (list 0 (make-real 1)))))
(define p2 (make-polynomial 'x (list (list 3 (make-real 1)) (list 0 (make-real 1))))) 
;The definition of p1 and p2 are not the same as what is given in Exercise 2.93.
;I think by default the exercise assume you to use "scheme-number". And by adjust the definition of "tag-type" etc,
;pure numbers automatically have types (it is like what we did in Exercise 2.79--2.80).
;In my case, I only have "real" and it NEED TO have type tags. So I change the definitions of p1 and p2 as these.

(greatest-common-divisor (make-real 8) (make-real 12)) ;'(real . 4)
(greatest-common-divisor p1 p2) ;okay

(define rn (make-rational (make-real 3) (make-real 4)))
(add rn rn) ;'(rational (real . 3) real . 2)

(define rf (make-rational p2 p1))
(add rf rf) ;okay and successfully simplified:-)

;Exercise 2.94

(define q1 (make-polynomial 'x (list (list 4 (make-real 1))
                                     (list 3 (make-real -1))
                                     (list 2 (make-real -2))
                                     (list 1 (make-real 2)))))
(define q2 (make-polynomial 'x (list (list 3 (make-real 1)) (list 1 (make-real -1))))) 
(greatest-common-divisor q1 q2) ;'(polynomial x (2 (real . -1)) (1 (real . 1)))

;Exercise 2.95

(define r1 (make-polynomial 'x (list (list 2 (make-real 1))
                                     (list 1 (make-real -2))
                                     (list 0 (make-real 1)))))
(define r2 (make-polynomial 'x (list (list 2 (make-real 11)) (list 0 (make-real 7))))) 
(define r3 (make-polynomial 'x (list (list 1 (make-real 13)) (list 0 (make-real 5))))) 
(greatest-common-divisor (mul r1 r2) (mul r1 r3)) ;As the problem said, it is not r1. Aka, noninteger operations.