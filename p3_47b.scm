#lang racket

(define (make-mutex)
  (define (make-semaphore-recur n) (if (= n 0)
                                       '()
                                       (cons false (make-semaphore-recur (- n 1)))))
  (let ((cell (make-semaphore-recur n)))            
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell)
  (if (car cell)
      (set-car! cell false)
      (if (not (null? (cdr cell)))
          (clear! (cdr cell)))))
(define (test-and-set! cell)
  (cond ((and (car cell) (null? (cdr cell))) true)
        ((car cell) (test-and-set! (cdr cell)))
        (else (begin (set-car! cell true)
                     false))))