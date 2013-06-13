#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;;;

(define (reverse torev-seq done-seq)
  (if (eq? torev-seq '())
      done-seq
      (reverse (cdr torev-seq) (cons (car torev-seq) done-seq))))

(define (union-set tree1 tree2)
  (define (union-set-tmp set-got set1 set2)
    (cond ((null? set1) (reverse set-got set2))
          ((null? set2) (reverse set-got set1))
          (else (let ([x1 (car set1)]
                      [x2 (car set2)])
                   (cond ((> x1 x2) (union-set-tmp (cons x2 set-got) set1 (cdr set2)))
                         ((< x1 x2) (union-set-tmp (cons x1 set-got) (cdr set1) set2))
                         ((= x1 x2) (union-set-tmp (cons x2 set-got) (cdr set1) (cdr set2))))))))
  (list->tree (union-set-tmp '() (tree->list tree1) (tree->list tree2))))

(define (intersection-set tree1 tree2)
  (define (intersection-set-tmp set1 set2)
    (if (or (null? set1) (null? set2))
        '()    
        (let ((x1 (car set1)) (x2 (car set2)))
          (cond ((= x1 x2)
                 (cons x1
                       (intersection-set-tmp (cdr set1)
                                             (cdr set2))))
                ((< x1 x2)
                 (intersection-set-tmp (cdr set1) set2))
                ((< x2 x1)
                 (intersection-set-tmp set1 (cdr set2)))))))
  (list->tree (intersection-set-tmp (tree->list tree1) (tree->list tree2))))

(define tree1 (list->tree (list 1 2 3 5 6 8 9)))
(define tree2 (list->tree (list 2 3 4 8 9)))
(union-set tree1 tree2) ;'(4 (2 (1 () ()) (3 () ())) (6 (5 () ()) (8 () (9 () ()))))
(intersection-set tree1 tree2) ;'(3 (2 () ()) (8 () (9 () ())))

;The implementation is O(n), because every middle step procedure (tree->list => union/intersection-set-tmp => list->tree) has
;implementation O(n). However, is there another (better) way which avoid converting the tree to a list to do that?