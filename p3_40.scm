#lang racket

(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))

;Possibilities
;(* (* 10 10 10) (* 10 10 10)) = 1000000
;(* (* 10 10) (* 10 10) (* 10 10)) = 1000000
;(* 10 (* 10 10 10)) = 10000
;(* 10 (* 10 10) (* 10 10)) = 100000
;(* 10 10 (* 10 10)) = 10000
;(* 10 10 10) = 1000
;(* 10 10) = 100

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define x 10)

(define s (make-serializer))

(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))

;only the top two possibilities remain.