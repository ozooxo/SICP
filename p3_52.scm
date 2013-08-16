#lang racket

(require racket/stream)

(define the-empty-stream empty-stream)
(define stream-null? stream-empty?)
(define stream-car stream-first)
(define stream-cdr stream-rest)

;;;

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (stream-cons
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (display-stream s)
  (stream-for-each (lambda (s) 
                     (display s)
                     (newline))
                   s))

(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum) ;sum = 0
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
sum ;0
(define y (stream-filter even? seq))
sum ;0
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
sum ;0
; As they are just definitions. They didn't really force anything

;(stream-enumerate-interval 1 20) = '(1 2 3 ... 20)
;seq = '(1 (1+2)*2/2 ... (1+16)*16/2 ...)

(stream-ref y 7) ;(1+16)*16/2 = 136
sum ;(1+16)*16/2 = 136
(display-stream z) ;elements in seq which can mod 5.
;10 = (4+1)*4/2
;15
;45
;55
;105
;120
;190
;210 = (20+1)*20/2