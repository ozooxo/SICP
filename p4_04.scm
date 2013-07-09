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
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((and? exp) (eval-and exp env)) ;;;;;;;;;;;;;;;;;;;;;;;;;
        ((or? exp) (eval-or exp env)) ;;;;;;;;;;;;;;;;;;;;;;;;;
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))

(define (eval-and exp env)
  (define (eval-and-recur exp env)
    (cond ((null? exp) true)
          ((not (true? (eval (car exp) env))) false)
          (else (eval-and-recur (cdr exp) env))))
  (eval-and-recur exp env))

(define (eval-or exp env)
  (define (eval-or-recur exp env)
    (cond ((null? exp) false)
          ((true? (eval (car exp) env)) true)
          (else (eval-or-recur (cdr exp) env))))
  (eval-or-recur exp env))