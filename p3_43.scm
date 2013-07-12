#lang racket

;If the processes are run sequentially, the account balances are just the module of the 
;permutation group, as the permutative actions act one by one.

;If each bank account has its own serializer, then there is no way to change the balance of 
;an account in the process of
(if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")
;Therefore, money always need to go from somewhere to somewhere else, and the total amount
;is conserved. However, things may happen while calculating the difference of two bank accounts
;and decide the amount which need to be deposit/withdraw, so anomalous behaviors (such as
;negative balances, process doesn't really exchange the amount of two balances) may happens
;in the process.