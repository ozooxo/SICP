#lang racket

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;Not exactly sure. "tree->list-1" should be more slower, but that depend on the implementation of "(append xx)".
;If "(append xx)" has an implementation of O(n**2) as we constructed from "accumulate", then "tree->list-1" has the
;implementation O(n**2) + O(n) + O(log n) = O(n**2) -- O(n) for "(cons (entry tree) xx)" and O(log n) for survey down.
;But if "append" has an O(n) implementation, the entire process is O(n).
;"tree->list-2" should have an implementation of O(n).