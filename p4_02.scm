#lang racket

;Louis's plan doesn't work, because "(application? '(define x 3))" is yes, and it will
;excute "(apply (eval 'define env) (list-of-values '(x 3) env))".
;As 'define is a variable, and '(x 3) is a pure list, if we farther define 
;"(lookup-variable-value 'define env)" as "define", it is actually "(apply define '(x 3))",
;which is "(define x 3)". So it goes as an infinite loop.

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))

;Then his plan is working.