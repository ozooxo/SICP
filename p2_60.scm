#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

;;;

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set))))) ;No change.

(define (adjoin-set x set) (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2)))) ;No change.

(define (union-set set1 set2) (append set1 set2))

(define s1 (list 1 2 2 3))
(define s2 (list 2 2 3 4))
(element-of-set? 2 s1) ;#t
(element-of-set? 5 s1) ;#f
(intersection-set s1 s2) ;'(2 2 3)
(union-set s1 s2) ;'(1 2 2 3 2 2 3 4)

; "adjoin-set" and "union-set" are much simplier, but the codes are much less efficient,
; because the representation list is much longer, and every procedure need to go through
; the list for several times.
