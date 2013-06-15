#lang racket

(define (make-account balance password)
  (let ([password-list (list password)])
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (add-password pw)
      (set! password-list (cons pw password-list)))
    (define (check-password pw pwlist)
      (cond ((eq? pwlist '()) false)
            ((eq? pw (car pwlist)) true)
            (else (check-password pw (cdr pwlist)))))
    (define (dispatch pw m)
      (if (check-password pw password-list)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                ((eq? m 'add-password) add-password)
                (else (error "Unknown request -- MAKE-ACCOUNT" m)))
          (lambda (arg) (display "Incorrect password"))))
    dispatch))

(define (make-joint account password new-password)
  (begin ((account password 'add-password) new-password)
         account))

(define peter-acc (make-account 100 'open-sesame))
((peter-acc 'open-sesame 'withdraw) 10) ;90

(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
((paul-acc 'rosebud 'withdraw) 10) ;80
((peter-acc 'open-sesame 'withdraw) 10) ;70
((paul-acc 'wrong-password 'withdraw) 10) ;Incorrect password

((peter-acc 'rosebud 'withdraw) 10) ;60
;But Peter can use Paul's password, and Paul can use Peter's password. The current code have nothing to 
;do about this situation. I think this task should be non-trivial, because when we define "peter-acc"
;to be "(make-account 100 'open-sesame)", the account itself doesn't really know that the account name
;is "peter-acc".
