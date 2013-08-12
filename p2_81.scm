#lang racket

(define *table* (make-hash))
(define (get op type) (hash-ref *table* (list op type) false))
(define (put op type val) (hash-set! *table* (list op type) val))
;In my original solutions (problem 73-80), there is no "false" as the default failure result.
;So in case to get something which is not exist, the program doesn't go to
;"No method for these types" in "apply-generic", but directly goes to an error.
;So I currect the code here.

(define *table-coercion* (make-hash))
(define (get-coercion op type) (hash-ref *table-coercion* (list op type) false))
(define (put-coercion op type val) (hash-set! *table-coercion* (list op type) val))

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

;;;

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2);;;
                    (error "No method for these types" (list op type-tags));;; changed as asked for (c)
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else
                             (error "No method for these types" (list op type-tags)))))))
              (error "No method for these types" (list op type-tags)))))))

;;;

(define (square x) (* x x))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-rectangular-package)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;;;

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))    
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (+ x y)))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (- x y)))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (* x y)))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (/ x y)))
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (expt x y)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'make 'scheme-number
       (lambda (x) x))
  'done)

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex)
       (lambda (z1 z2) (and (= (real-part z1) (real-part z2)) (= (imag-part z1) (imag-part z2)))))
  (put '=zero? '(complex)
       (lambda (z1) (= (magnitude z1) 0)))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;;;

(install-scheme-number-package)
(install-complex-package)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

;;;

(define (scheme-number->complex n) (make-complex-from-real-imag (contents n) 0))
(put-coercion 'scheme-number 'complex scheme-number->complex)

;;;

(add 3 4) ;7
(add (make-complex-from-real-imag 1 5) (make-complex-from-real-imag 6 7)) ;'(complex rectangular 7 . 12)
(add 3 (make-complex-from-real-imag 6 7)) ;'(complex rectangular 9 . 7)
;All of them work.

;;;

;(a)

;(define (scheme-number->scheme-number n) n)
;(define (complex->complex z) z)
;(put-coercion 'scheme-number 'scheme-number
;              scheme-number->scheme-number)
;(put-coercion 'complex 'complex complex->complex)

(define (exp x y) (apply-generic 'exp x y))
(exp 2 3) ;8
(exp 3 (make-complex-from-real-imag 6 7)) ;for this case, if for the original code (without Louis's changes),
                                          ;it will just get an error message "No method for these types
                                          ;(exp (complex complex))". However, with Louis's changes, it will
                                          ;goes an infinite loop. When apply-generic cannot find
                                          ;('exp ('complex 'complex)), it will try to find it forever.
(exp (make-complex-from-real-imag 1 5) (make-complex-from-real-imag 6 7))

;(b)

;Louis is not right. Those kind of things are not necessary for "apply-generic".
;The reason is that "apply-generic" first try the default case. So if it is e.g. '(scheme-number scheme-number),
;"if proc" is true and the problem goes to "(apply proc (map contents args))",
;and type1 type2 will never be calculated.

;(c)

;See the changes down in "apply-generic".