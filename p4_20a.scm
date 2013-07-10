#lang racket

;Notics that "letrec" is actually predefined in Racket.

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;;;;; All above functions are copied from SICP ;;;;;

(define (letrec? exp)
  (tagged-list? exp 'letrec))

(define (letrec-parameter exp) (cadr exp))
(define (letrec-body exp) (cddr exp))

(define (make-set parameter-body-pair-as-lst)
  (cons 'set! parameter-body-pair-as-lst))
(define (make-let parameters body)
  (cons 'let (cons parameters body)))
(define (make-letrec parameters body)
  (cons 'letrec (cons parameters body)))

(define (transform-letrec-to-let exp)
  (if (letrec? exp)
      (make-let (map (lambda (x) (list (car x) ''*unassigned*)) (letrec-parameter exp))
                (append (map make-set  (letrec-parameter exp)) (letrec-body exp)))
      (error "Not a letrec function" exp)))

(transform-letrec-to-let '(letrec ((<var1> <exp1>) (<var2> <exp2>))
                            <body>))
'(let ((<var1> '*unassigned*) (<var2> '*unassigned*)) (set! <var1> <exp1>) (set! <var2> <exp2>) <body>)

;In general, it should transform
;(letrec ((<var1> <exp1>) ... (<varn> <expn>))
;  <body>)
;=========to========>
;(let ((<var1> '*unassigned*) ... (<varn '*unassigned*))
;  (set! <var1> <exp1>)
;  ...
;  (set! <varn> <expn>)
;  <body>)
       