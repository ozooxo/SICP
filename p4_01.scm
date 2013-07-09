#lang racket

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (list-of-values-from-left exps env)
  (if (no-operands? exps)
      '()
      (let ([fst (eval (first-operand exps) env)])
        (cons fst
              (list-of-values (rest-operands exps) env)))))

(define (list-of-values-from-right exps env)
  (if (no-operands? exps)
      '()
      (let ([snd (list-of-values (rest-operands exps) env)])
        (cons (eval (first-operand exps) env)
              snd))))