#lang racket

(define (make-account balance password)
  (let ([count-incorrect-password 0])
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (call-the-cops) (display "Call the cops!!"))
    (define (dispatch pw m) ;It doesn't work if I set "count-incorrect-password" as a local
                            ;variable of "dispatch", rather than "make-account".
                            ;I don't fully understand why.
      (if (eq? pw password)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Unknown request -- MAKE-ACCOUNT" m)))
          (lambda (arg) 
            (begin
              (set! count-incorrect-password (+ count-incorrect-password 1))
              (if (= count-incorrect-password 7)
                  (call-the-cops)
                  (begin
                    (display "Incorrect password")
                    (newline)))))))
  dispatch))

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40) ;60
((acc 'some-other-password 'deposit) 50) ;Incorrect password
((acc 'some-other-password 'deposit) 50) ;Incorrect password
((acc 'some-other-password 'deposit) 50) ;Incorrect password
((acc 'some-other-password 'deposit) 50) ;Incorrect password
((acc 'some-other-password 'deposit) 50) ;Incorrect password
((acc 'some-other-password 'deposit) 50) ;Incorrect password
((acc 'some-other-password 'deposit) 50) ;Call the cops!!
