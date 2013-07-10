#lang racket

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;;;;; All above functions are copied from SICP ;;;;;

(define (make-set parameter-body-pair-as-lst)
  (cons 'set! parameter-body-pair-as-lst))
(define (make-let parameters body)
  (cons 'let (cons parameters body)))

;My "scan-out-defines" is not limited to the function "lambda".
;Based on Exercise 4.16c, the use seems to be limited for "lambda" (however, my solution of 4.16c need it NOT to be this case).
(define (scan-out-defines exp)
  (define (scan-out-defines-iter if-met-defines exps-before-defines define-pairs exp)
    (cond ((and (eq? if-met-defines false) (not (definition? (car exp))))
           (scan-out-defines-iter false (cons (car exp) exps-before-defines) define-pairs (cdr exp)))
          ((definition? (car exp))
           (scan-out-defines-iter true exps-before-defines (cons (list (definition-variable (car exp))
                                                                       (definition-value (car exp))) define-pairs) (cdr exp)))
          ((and (eq? if-met-defines true) (or (null? exp) (not (definition? (car exp)))))
           (append (reverse exps-before-defines)
                   (list (make-let (map (lambda (x) (list (car x) ''*unassigned*)) define-pairs) ;based on the example given in SICP,
                                                                                                 ;it need to be ''*unassigned*.
                                   (append (map make-set define-pairs) exp)))))))
  (scan-out-defines-iter false '() '() exp))

(scan-out-defines '(lambda <vars>
                     (define u <e1>)
                     (define v <e2>)
                     <e3>))
;'(lambda <vars> (let ((v '*unassigned*) (u '*unassigned*)) (set! v <e2>) (set! u <e1>) <e3>))