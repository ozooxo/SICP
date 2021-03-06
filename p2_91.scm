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
  'done)

(install-real-package)

(define (make-real x) ((get 'make 'real) x))

;(add (make-real 2.3) (make-real 5)) ;'(real . 7.3)

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
  (define (div-poly p1 p2) 
    (if (same-variable? (variable p1) (variable p2))
        (list (make-poly (variable p1)
                         (car (div-terms (term-list p1)
                                         (term-list p2))))
              (make-poly (variable p1)
                         (cadr (div-terms (term-list p1)
                                         (term-list p2)))))
        (error "Polys not in same var -- DIV-POLY" (list p1 p2))))
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
  (put 'div '(polynomial polynomial) (lambda (p1 p2) (list (tag (car (div-poly p1 p2))) (tag (cadr (div-poly p1 p2))))))
  (put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms))))
  (put 'neg '(polynomial) (lambda (x) (tag (neg-poly x))))
  (put 'sub '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 (neg-poly p2)))))
  (put '=zero? '(polynomial) =zero?-poly)
  'done)

(install-polynomial-package)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(define p1 (make-polynomial 'x (list (list 5 (make-real 6))
                                     (list 2 (make-real 2))
                                     (list 1 (make-real 1))
                                     (list 1 (make-real 3)))))
(define p2 (make-polynomial 'x (list (list 3 (make-real 3))
                                     (list 2 (make-real 5)))))
(div p1 p2) ;okay