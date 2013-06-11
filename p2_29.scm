#lang racket

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (total-weight mobile)
  (if (number? mobile)
      mobile
      (+ (total-weight (branch-structure (left-branch mobile))) (total-weight (branch-structure (right-branch mobile))))))

(define (balanced mobile)
  (if (number? mobile)
      true
      (and (= (* (branch-length (left-branch mobile)) (total-weight (branch-structure (left-branch mobile))))
              (* (branch-length (right-branch mobile)) (total-weight (branch-structure (right-branch mobile)))))
           (balanced (branch-structure (left-branch mobile)))
           (balanced (branch-structure (right-branch mobile))))))

(define m1 3)
(define m2 6)
(define b1 (make-branch 4 m1))
(define b2 (make-branch 2 m2))
(define m3 (make-branch b1 b2))

(total-weight m3) ;9
(balanced m3) ;#t

; For problem (d), only "right-branch" and "branch-structure" need to be changed from "(car (cdr ...))" to "(cdr ...)".