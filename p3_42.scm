#lang racket

;It doesn't work, because right now, (make-serializer) is protected the function body of "withdraw"
;and "deposit", i.e., the procedures
(if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")
;etc, which are the same all the time.