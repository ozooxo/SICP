#lang racket

(require racket/stream)

(define the-empty-stream empty-stream)
(define stream-null? stream-empty?)
(define stream-car stream-first)
(define stream-cdr stream-rest)

(define (stream-take n s)
  (if (= n 0)
      '()
      (cons (stream-car s) (stream-take (- n 1) (stream-cdr s)))))

;;;

;Exercise 3.63

(define (average x y) (/ (+ x y) 2))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream1 x)
  (define guesses
    (stream-cons 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(stream-take 5 (sqrt-stream1 2)) ;'(1.0 1.5 1.4166666666666665 1.4142156862745097 1.4142135623746899)

(define (sqrt-stream2 x)
  (stream-cons 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream2 x))))

(stream-take 5 (sqrt-stream2 2)) ;'(1.0 1.5 1.4166666666666665 1.4142156862745097 1.4142135623746899)

;If use "delay" as "(lambda () <exp>)", then no difference.
;The major difference between "sqrt-stream1" and "sqrt-stream2" is that,
;the recursion of "sqrt-stream1" is inside of the body of "sqrt-stream1", but the recursion
;of "sqrt-stream2" calls "(sqrt-stream2 x)" everytime.
;When it need to cal "(sqrt-stream2 x)" with some particular value of x, "memo-proc" cannot remember
;the value in "(sqrt-stream2 x)", hence less efficient.

;P.S I don't really think about this problem carefully. I believe that the question highly depend on the
;compiler of the programming language. Perhaps for some for modern functional programming language like
;Haskell, it can automatically accelerate the calculating process by other ways to memorize the middle 
;step results.

;;;

;Exercise 3.64

(define (stream-limit s tolerance)
  (if (stream-null? (stream-cdr s))
      (error "not an infinite stream" s)
      (let ([x1 (stream-car s)]
            [x2 (stream-car (stream-cdr s))])
        (if (< (abs (- x1 x2)) tolerance)
            x2
            (stream-limit (stream-cdr s) tolerance)))))
 
(define (sqrt x tolerance)
  (stream-limit (sqrt-stream1 x) tolerance))

(sqrt 2 0.0001) ;1.4142135623746899