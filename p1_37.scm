#lang racket

(define (cont-frac n d k) 
  (if (< k 2)
      (/ (n 0) (d 0)) ; In here, the very bottom one is n_0, d_0, where the top one is n_k, t_k.
                      ; It is not exactly the same as the equation defined. But for "(lambda (i) 1.0)", it is fine.
      (/ (n k) (+ (d k) (cont-frac n d (- k 1))))))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10) ;0.6179775280898876
               ;It is accurate to 4 decimal places compare to the real value 1.61803398875 

;;;

(define (cont-frac-iter n d k)
  (define (cont-frac-iter-tmp n d k result) 
    (if (< k 1)
        result
        (cont-frac-iter-tmp n d (- k 1) (/ (n k) (+ (d k) result)))))
  (cont-frac-iter-tmp n d k 0))

(cont-frac-iter (lambda (i) 1.0)
                (lambda (i) 1.0)
                10) ;0.6179775280898876