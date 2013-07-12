#lang racket

(define (eval exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((let? exp) (analyze-let exp)) ;;;;;;;;;;;;;;;;;;;;;;;
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (let? exp) (tagged-list? exp 'let))

(define (let-parameters exp) (cadr exp))
(define (let-var exp) (map car exp))
(define (let-exp exp) (map cadr exp))
(define (let-body exp) (cddr exp))

(define (analyze-let exp)
  (let ((proc (analyze (cons (make-lambda (let-var exp) (let-body exp)) (let-exp exp)))))
    (lambda (env) (proc env))))