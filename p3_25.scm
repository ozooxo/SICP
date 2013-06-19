#lang racket

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (mcar (mcar records))) (mcar records))
        (else (assoc key (mcdr records)))))

(define (make-table)
  (let ([table (mcons '*table* '())])
    (define (lookup . key)
      (let ((record (assoc key (mcdr table))))
        (if record
            (mcdr record)
            false)))
    (define (insert! value . key) ;I change the order of the arguments key and value here.
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

(put 'data1 'key1 'key2) ;'ok
(put 'data2 'key3 'key4 'key5) ;'ok
(get 'key1) ;#f
(get 'key1 'key2) ;'data1
(get 'key3 'key4 'key5) ;'data2