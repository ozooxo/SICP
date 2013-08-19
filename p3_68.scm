#lang racket

;<support functions>

(define (pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (pairs (stream-cdr s) (stream-cdr t))))

(stream-take 15 (pairs integers integers))
;It doesn't work. Because it try to merge '(<s0t0> <s0t1> <s0t2> ...)
;with '(<s1t1> <s1t2> ...) with '(<s2t2> <s2t3> ...) with ... (merging from right to left)
;hence, it will try to find the first element of '(<sntn,n->infty> ...) forever, and never 
;merge to the region we are interested in.