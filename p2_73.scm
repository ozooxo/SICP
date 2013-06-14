#lang racket

(define *table* (make-hash))
(define (get op type) (hash-ref *table* (list op type)))
(define (put op type val) (hash-set! *table* (list op type) val))

;;;

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (base p) (cadr p))
(define (exponent p) (caddr p))
(define (make-exponentiation x n)
  (cond ((=number? n 0) 1)
        ((=number? n 1) x)
        (else (list '** x n))))

;;;

(define (install-sum-package)
  (define (deriv-inner exp var) ;In here, the procedure need a different name rather than just "deriv", because "deriv" need to
                                ;call the outer process rather than just recurse itself.
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  (put 'deriv '+ deriv-inner))

(define (install-product-package)
  (define (deriv-inner exp var)
    (make-sum
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp))))
  (put 'deriv '* deriv-inner))

(define (install-exponentiation-package)
  (define (deriv-inner exp var)
    (make-product (exponent exp)
                  (make-exponentiation (base exp) (- (exponent exp) 1))))
  (put 'deriv '** deriv-inner))

;;;  

(install-sum-package)
(install-product-package)
(install-exponentiation-package)

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) exp ;In here it is "exp" rather than "(operands exp)" as in the exercise guide.
                                                ;We can also use "(operands exp)" here, but then we need to change the definitions
                                                ;of addend/augend/multiplier/multiplicand
                                            var))))

(deriv '(+ x 3) 'x) ;1
(deriv '(* x y) 'x) ;'y
(deriv '(* (* x y) (+ x 3)) 'x) ;'(+ (* x y) (* y (+ x 3)))
(deriv '(** (+ (* 2 x) y) 3) 'x) ;'(* 3 (** (+ (* 2 x) y) 2))

;;;

;(a)
;Because the information how to do "deriv" for numbers and variables are not in the hash.
;"(operator exp)" will give error message if exp is a number or a variable.

;(d)
;if we use "((get (operator exp) 'deriv) (operands exp) var)", i.e. (get <type> <op>),
;then we need to also switch the order for input to (put <type> <op> <item>).