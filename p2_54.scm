#lang racket

(define (equal? list1 list2)
  (cond ((and (null? list1) (null? list2)) true)
        ((or (and (null? list1) (not (null? list2)))
             (and (null? list2) (not (null? list1)))) false) ;Is there a better way for this case?
        ((not (eq? (car list1) (car list2))) false)
        (else (equal? (cdr list1) (cdr list2)))))
;BTW, "equal?" is a build-in function is Racket. We overwrite it here.

(equal? '(this is a list) '(this is a list)) ;#t
(equal? '(this is a list) '(this is a list or not)) ;#f
(equal? '(this is a list) '(this is a cat)) ;#f
