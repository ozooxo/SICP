#lang racket

(define how-many-account 0)

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((account-num how-many-account) ;;;;;;;;;;;;;;;;
        (protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) balance)
            ((eq? m 'account-num) account-num) ;;;;;;;;;;;;;;;;
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    (begin
      (set! how-many-account (+ how-many-account 1)) ;;;;;;;;;;;;;;;;
      dispatch)))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (if (> (account1 'account-num) (account2 'account-num))
        ((serializer1 (serializer2 exchange))
         account1
         account2)
        ((serializer2 (serializer1 exchange))
         account1
         account2))))