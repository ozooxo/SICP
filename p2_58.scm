#lang racket

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (binary-before operator exp)
  (define (binary-before-tmp exp)
    (if (eq? (car exp) operator)
        '()
        (cons (car exp) (binary-before-tmp (cdr exp)))))
  (let ([tmp (binary-before-tmp exp)])
    (if (eq? (cdr tmp) '())
        (car tmp)
        tmp)))
(define (binary-after operator exp)
  (define (binary-after-tmp exp)
    (if (eq? (car exp) operator)
        (cdr exp)
        (binary-after-tmp (cdr exp))))
  (let ([tmp (binary-after-tmp exp)])
    (if (eq? (cdr tmp) '())
        (car tmp)
        tmp)))

(define (sum? x)
  (cond ((not (pair? x)) false)
        ((eq? (car x) '+) true)
        (else (sum? (cdr x)))))
(define (addend s) (binary-before '+ s))
(define (augend s) (binary-after '+ s))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (product? x)
  (define (no-sum x)
    (cond ((not (pair? x)) true)
          ((eq? (car x) '+) false)
          (else (no-sum (cdr x)))))
  (define (has-product x)
    (cond ((not (pair? x)) false)
          ((eq? (car x) '*) true)
          (else (has-product (cdr x)))))
  (if (eq? (no-sum x) true)
      (has-product x)
      false))
(define (multiplier p) (binary-before '* p))
(define (multiplicand p) (binary-after '* p))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

;(sum? '(a + b + c)) ;#t
;(sum? '(a * b * c)) ;#f
;(addend '(a + b + c)) ;'a
;(addend '(a * b + c)) ;'(a * b)
;(augend '(a + b + c)) ;'(b + c)
;(augend '(a * b + c * d)) ;'(c * d)
;(product? '(a * b * c)) ;#t
;(product? '(a * b + c)) ;#f
;(product? '(a + b * c)) ;#f

(deriv '(x + (3 * (x + (y + 2)))) 'x) ;4
(deriv '(x + 3 * (x + y + 2)) 'x) ;4