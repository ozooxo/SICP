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
        ((let? exp) (let->combination exp env)) ;;;;;;;;;;;;;;;;;;;;;
        ((let*? exp) (eval (let*->nested-lets exp) env)) ;;;;;;;;;;;;;;;;;;;
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (let? exp) (tagged-list? exp 'let))
(define (let*? exp) (tagged-list? exp 'let*))

(define (let-parameters exp) (cadr exp))
(define (let-var exp) (map car exp))
(define (let-exp exp) (map cadr exp))
(define (let-body exp) (cddr exp))
(define (let*-parameters exp) (cadr exp))
(define (let*-body exp) (cddr exp))

(define (make-let parameters body)
  (cons 'let (cons parameters body)))
(define (make-let* parameters body)
  (cons 'let* (cons parameters body)))

(define (let->combination exp env)
  (eval (cons (make-lambda (let-var exp) (let-body exp)) (let-exp exp)) env))

(define (let*->nested-lets exp)
  (if (null? (cdr (let*-parameter exp)))
      (make-let (let*-parameter exp) (let*-body exp))
      (make-let (cons (car (let*-parameter exp)) '())
                (let*-nested-let (make-let* (cdr (let*-parameter exp))) (let*-body exp)))))
;So, it is sufficient to add a clause to eval whose action is "(eval (let*->nested-lets exp) env)".