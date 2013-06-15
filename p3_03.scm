#lang racket

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pw m)
    (if (eq? pw password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT" m)))
        (lambda (arg) (display "Incorrect password")))) ;This anonymous function aims to absorb the parameter giving
                                                        ;to "dispatch". If we just do "(display "Incorrect password")", 
                                                        ;then the withdraw/deposit amount has nowhere to go, therefore 
                                                        ;cause errors.
  dispatch)

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40) ;60
((acc 'some-other-password 'deposit) 50) ;Incorrect password
