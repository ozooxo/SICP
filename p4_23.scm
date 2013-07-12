#lang racket

;text version
(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env))) ;Is that right?
                                            ;The lambda function has 2 return values???
                                            ;??????????
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

;Alyssa P. Hacker's version
(define (analyze-sequence exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs)) ((car procs) env))
          (else ((car procs) env)
                (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (lambda (env) (execute-sequence procs env))))

;If the sequence has only one expression, than "exps" is "(exp, '())".
;Test version gives 
;(loop (analyze exp) '()) 
;== (analyze exp)
;and Alyssa's version gives 
;(lambda (env) (execute-sequence ((analyze exp) '()) env))
;== (lambda (env) ((analyze exp) env)
;== (analyze exp)

;If "exps == (exp1, exp2, '())"
;Test version gives
;(loop (analyze exp1) ((analyze exp2) '()))
;== (loop (sequentially (analyze exp1) (analyze exp2)) '())
;== (sequentially (analyze exp1) (analyze exp2))
;== (lambda (env) ((analyze exp1) env) ((analyze exp2) env))
;and Alyssa's version gives
;(lambda (env) (execute-sequence ((analyze exp1) (analyze exp2) '()) env)
;== (lambda (env) ((analyze exp1) env) ((analyze exp2) env))

;I firstly don't understand why lambda (in both "analyze-sequence" functions) can return two values
;(which is NOT a list of two values). I secondly don't see why Alyssa's method "In effect, although
;the individual expressions in the sequence have been analyzed, the sequence itself has not been."

;I thought one problem is that for the series of expressions in "begin", some of them may "set!"
;and they should change "env" before the follow up expressions are evaluated.
;However, I think even 
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))
;doesn't really care about this effect. "env" comes as an argument of "eval-sequence", and it will
;not change for the whole recursive process.