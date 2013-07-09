#lang racket

(define *table* (make-hash))
(define (can-get? op type) (hash-has-key? *table* (list op type)))
(define (get op type) (hash-ref *table* (list op type)))
(define (put op type val) (hash-set! *table* (list op type) val))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-eval-package)
  (put 'eval 'quote (lambda (exp env) (text-of-quotation exp)))
  (put 'eval 'set! eval-assignment)
  (put 'eval 'define eval-definition)
  (put 'eval 'if eval-if)
  (define (eval-lambda exp env)
    (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
  (put 'eval 'lambda eval-lambda)
  (define (eval-begin exp env)
    (eval-sequence (begin-actions exp) env))
  (put 'eval 'begin eval-begin)
  (define (eval-cond exp env)
    (eval (cond->if exp) env))
  (put 'eval 'cond eval-cond)

(install-eval-package)
  
(define (eval exp env)
   (cond ((self-evaluating? exp) exp)
         ((variable? exp) (lookup-variable-value exp env))
         ((can-get? 'eval (operator exp))
          ((get 'eval (operator exp)) exp var))
         ((application? exp)
          (apply (eval (operator exp) env)
                 (list-of-values (operands exp) env)))
         (else
          (error "Unknown expression type -- EVAL" exp))))