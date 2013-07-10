#lang racket

(define (run-forever) (run-forever))

(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))

;Thank about the expression "(try try)". If it is halted, then "(halts? try try)" is true,
;so "(try try)" will "(run-forever)". If "(try try)" is not halted, then "(halts? try try)"
;is false, so "(try try)" will be "'halted".