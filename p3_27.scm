#lang racket

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (mcar (mcar records))) (mcar records))
        (else (assoc key (mcdr records)))))

(define (lookup key table)
  (let ((record (assoc key (mcdr table))))
    (if record
        (mcdr record)
        false)))

(define (insert! key value table)
  (let ((record (assoc key (mcdr table))))
    (if record
        (set-mcdr! record value)
        (set-mcdr! table
                   (mcons (mcons key value) (mcdr table)))))
  'ok)

(define (make-table)
  (mcons '*table* '()))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

(memo-fib 6)
;The implementation is O(n), because every (memo-fib n) need to be caculated only once.
;For example, when we calculate (memo-fib n), we need (memo-fib (- n 1)) and 
;(memo-fib (- n 2)), but when we ask the value of (memo-fib (- n 2)), we already know its
;value from the calculation of (memo-fib (- n 1)) calculation, hence we don't need to do
;it again.

;Simply defined memo-fib as (memoize fib) doesn't work, because inside of (fib n)
;we use (fib (- n 1)) rather than (memo-fib (- n 1)) so it will never go to look up the
;value in the memoizated table. However, if we change (fib n) to
;(define (fib n)
;  (cond ((= n 0) 0)
;        ((= n 1) 1)
;        (else (+ (memo-fib (- n 1))
;                 (memo-fib (- n 2))))))
;then it should work.
