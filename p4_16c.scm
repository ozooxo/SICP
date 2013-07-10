#lang racket

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

(define (procedure-body p) (scan-out-defines (caddr p)))