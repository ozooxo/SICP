#lang racket

(define (scan-general vars vals
                      var env
                      todo-if-scan-null todo-if-scan-eq)
  (cond ((null? vars)
         (todo-if-scan-null vars vals var env))
        ((eq? var (car vars))
         (todo-if-scan-eq vars vals var env)) ;;;;;;;;;;;;;;;;;;
        (else (scan-general (cdr vars) (cdr vals)
                            var env todo-if-scan-null
                            todo-if-scan-eq))))

(define (scan-from-env scan env)
  (let ((frame (first-frame env)))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (env-loop var env todo-if-scan-eq)
  (define (scan vars vals)
    (scan-general vars vals
                  var env
                  (lambda (vars vals var env) (env-loop (enclosing-environment env))) todo-if-scan-eq))
  (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (scan-from-env scan env)))

(define (lookup-variable-value var env)
  (env-loop var env (lambda (vars vals var env) (car vals))))

(define (set-variable-value! var val env)
  (env-loop var env (lambda (vars vals var env) (set-car! vals val))))

(define (define-variable! var val env)
  (define (scan vars vals)
    (scan-general vars vals
                  var env
                  (lambda (vars vals var env) (add-binding-to-frame! var val (first-frame env)))
                  (lambda (vars vals var env) (set-car! vals val))))
  (scan-from-env scan env))