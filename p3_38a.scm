#lang racket

(define balance 100)

(set! balance (+ balance 10))
(set! balance (- balance 20))
(set! balance (- balance (/ balance 2)))
(display balance) ;45
(newline)

(set! balance 100)

(set! balance (- balance 20))
(set! balance (+ balance 10))
(set! balance (- balance (/ balance 2)))
(display balance) ;45
(newline)

(set! balance 100)

(set! balance (- balance 20))
(set! balance (- balance (/ balance 2)))
(set! balance (+ balance 10))
(display balance) ;50
(newline)

(set! balance 100)

(set! balance (- balance (/ balance 2)))
(set! balance (- balance 20))
(set! balance (+ balance 10))
(display balance) ;40
(newline)

(set! balance 100)

(set! balance (+ balance 10))
(set! balance (- balance (/ balance 2)))
(set! balance (- balance 20))
(display balance) ;35
(newline)

(set! balance 100)

(set! balance (- balance (/ balance 2)))
(set! balance (+ balance 10))
(set! balance (- balance 20))
(display balance) ;40
(newline)