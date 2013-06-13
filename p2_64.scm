#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))                      ;left-size = 3
        (let ((left-result (partial-tree elts left-size)))         ;left-result = '((2 (1 () ()) (3 () ())) 4 5 6 7)
          (let ((left-tree (car left-result))                      ;left-tree = '(2 (1 () ()) (3 () ()))
                (non-left-elts (cdr left-result))                  ;non-left-element = '(4 5 6 7)
                (right-size (- n (+ left-size 1))))                ;right-size = 3
            (let ((this-entry (car non-left-elts))                 ;this-entry = '4
                  (right-result (partial-tree (cdr non-left-elts)  ;right-result = '((6 (5 () ()) (7 () ())))
                                              right-size)))
              (let ((right-tree (car right-result))                ;right-tree = '(6 (5 () ()) (7 () ()))
                    (remaining-elts (cdr right-result)))           ;remaining-elts = '() in this case. But for general mid-step e.g.
                                                                   ;(partial-tree (list 1 2 3 4 5 6 7) 3), it holds the remain terms.
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(partial-tree (list 1 2 3 4 5 6 7) 7) ;'((4 (2 (1 () ()) (3 () ())) (6 (5 () ()) (7 () ()))))
;         4
;     2       6
;  1   3     5   7
(partial-tree (list 1 2 3 4 5 6 7 8) 8) ;'((4 (2 (1 () ()) (3 () ())) (6 (5 () ()) (7 () (8 () ())))))
;         4
;     2       6
;  1   3     5   7
;                  8

;(partial-tree (list 1 2 3 4 5 6 7) 3) ;'((2 (1 () ()) (3 () ())) 4 5 6 7)
;(partial-tree (list 5 6 7) 3) ;'((6 (5 () ()) (7 () ())))

;The implementation seems O(n). Not exactly sure. Are there a more regular way to calculate that?