#lang racket

(require racket/stream)

(define the-empty-stream empty-stream)
(define stream-null? stream-empty?)
(define stream-car stream-first)
(define stream-cdr stream-rest)

(define (stream-map-multiple-arguments proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car argstreams))
       (apply stream-map-multiple-arguments
              (cons proc (map stream-cdr argstreams))))))

(define (add-streams s1 s2)
  (stream-map-multiple-arguments + s1 s2))

(define (mul-streams s1 s2)
  (stream-map-multiple-arguments * s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (stream-take n s)
  (if (= n 0)
      '()
      (cons (stream-car s) (stream-take (- n 1) (stream-cdr s)))))

;;;

;Exercise 3.59(a)

(define (integrate-series s)
  (mul-streams s (stream-map (lambda (x) (/ 1 x)) integers)))

(define ones (stream-cons 1 ones))
(stream-take 7 (integrate-series ones)) ;okay

;Exercise 3.59(b)

(define exp-series
  (stream-cons 1 (integrate-series exp-series)))

(define (sum-list l)
  (cond [(null? l) 0]
        [else (+ (car l) (sum-list (cdr l)))]))
(exact->inexact (sum-list (stream-take 7 exp-series))) ;2.7180555555555554
(exp 1) ;2.718281828459045

(define cosine-series
  (stream-cons 1 (stream-map (lambda (x) (- 0 x)) (integrate-series sine-series))))
(define sine-series
  (stream-cons 0 (integrate-series cosine-series)))

(exact->inexact (sum-list (stream-take 7 sine-series))) ;0.8416666666666667
(sin 1) ;0.8414709848078965
(exact->inexact (sum-list (stream-take 7 cosine-series))) ;0.5402777777777777
(cos 1) ;0.5403023058681398

;;;

;Exercise 3.60

(define (mul-series s1 s2)
  (stream-cons (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series s2 (stream-cdr s1)))))

(stream-take 10 (mul-series ones ones)) ;'(1 2 3 4 5 6 7 8 9 10)
(stream-take 10 (mul-series integers integers)) ;'(1 (4=2*1+1*2) (10=3*1+2*2+1*3) 20 35 56 84 120 165 220)

(stream-take 10 (add-streams (mul-series cosine-series cosine-series)
                             (mul-series sine-series sine-series))) ;'(1 0 0 0 0 0 0 0 0 0)