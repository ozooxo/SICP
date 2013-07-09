#lang racket

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((while? exp) (eval-while exp env)) ;;;;;;;;;;;;;;;;;;;;;
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

;'(while <condition> <body>)

(define (while? exp) (tagged-list? exp 'while))
(define (while-condition exp) (cadr exp))
(define (while-body exp) (cddr exp))
(define (eval-while exp env)
  (define (while-iter exp env)
    (if (eq? (while-condition exp) true)
        (while-body exp)
        (while-iter (eval exp env) env))) ;It seems not correct. In order to have "while" in scheme,
                                          ;the <body> should contains a lot of "let" to renew the value.
                                          ;But it seems really hard for me to add/design an exact grammer
                                          ;to achieve that in the framework of scheme, as I still haven't 
                                          ;design what's the exact meaning to "let 'let' run once".
  (while-iter exp env))