#lang racket

(define (same-key? key1 key2)
  (let ([tolerance 0.1])
    (if (and (number? key1) (number? key2))
        (and (< key1 (+ key2 tolerance)) (> key1 (- key2 tolerance)))
        (equal? key1 key2))))

(define (assoc key records)
  (cond ((null? records) false)
        ((same-key? key (mcar (mcar records))) (mcar records))
        (else (assoc key (mcdr records)))))

(define (make-table)
  (let ([table (mcons '*table* '())])
    (define (lookup key)
      (let ((record (assoc key (mcdr table))))
        (if record
            (mcdr record)
            false)))
    (define (insert! key value)
      (let ((record (assoc key (mcdr table))))
        (if record
            (set-mcdr! record value)
            (set-mcdr! table
                       (mcons (mcons key value) (mcdr table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put 'a 3) ;'ok
(put 3.14 4) ;'ok
(get 'a) ;3
(get 3.15) ;4
(get 'c) ;#f
(get 3.25) ;#f