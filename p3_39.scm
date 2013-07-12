#lang racket

(define x 10)

(define s (make-serializer))

(parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
                  (s (lambda () (set! x (+ x 1)))))

;Possibilities
;first run (lambda () (set! x ((s (lambda () (* x x))))), second run (s (lambda () (set! x (+ x 1))))
;==> x = (+ (lambda () (* 10 10)) 1) = (+ 100 1) = 101
;first run (s (lambda () (set! x (+ x 1)))), second run (lambda () (set! x ((s (lambda () (* x x)))))
;==> x = (lambda () (* (+ 10 1) (+ 10 1))) = (lambda () 121) = 121
;run (lambda () (* x x)), then run (lambda () (set! x (+ x 1))), then run (lambda () (set! x ((s (lambda () (* x x)))))
;==> x = (lambda () (* 10 10)) = (lambda () 100) = 100
;run (set! x ((s (lambda () (* x x))))), then (lambda () (set! x (+ x 1))), then (lambda () (set! x ((s (lambda () (* x x)))))
;==> x = (+ (lambda () (* 10 10)) 1) = 101
;run (lambda () (* x x)), then (+ x 1) get x, then run (set! x ((s (lambda () (* x x))))),
;then run (lambda () (set! x (+ x 1)))), finally  (lambda () (set! x ((s (lambda () (* x x))))))
;==> x = (+ 10 1) = 11
;run (lambda () (* x x)), then (+ x 1) get x, then run (lambda () (set! x ((s (lambda () (* x x)))))),
;then run (lambda () (set! x (+ x 1))))
;==> x = (+ 10 1) = 11

;There seems only four (different) possibilities for x, 101, 121, 100, and 11???
;110 is not possible, as there is no way to change the value of x by the second expression, while running (* x x).

