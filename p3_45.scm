#lang racket

;The only difference between Louis's "make-account-and-serializer" and the original "make-account"
;at the beginning of section 3.4.2, is the "dispatch" has a new condition
((eq? m 'serializer) balance-serializer)
;As the deposit process
(define (deposit account amount)
 ((account 'deposit) amount))
;doesn't use
(account 'serializer)
;at all, that condition of "dispatch" has never been used, and his code has ALL the problems
;the original "make-account" have.
;Louis's function seems completely doesn't help for exchanging (since both of the accounts need to
;be interacted more than once, so sooner or later we need
(serializer1 (serializer2 exchange))
;However, for other special cases such as 
;account1 => 1.5 account1
;account2 => account2 - 0.5 account1
;we may use
(define (serial-proc account1 account2)
  (define (proc account1 account2)
    (let ((exchange (* 0.5 (account1 'balance))))
      ((account1 'deposit) exchange)
      ((account2 'withdraw) exchange)))
  (let ((serializer1 (account1 'serializer)))
    (serializer1 proc)))