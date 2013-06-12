#lang racket

(car ''abracadabra) ;'quote
(cdr ''abracadabra) ;'(abracadabra)

(equal? ''abracadabra (cons 'quote '(abracadabra))) ;t
(equal? ''abracadabra (cons 'quote (cons 'abracadabra '()))) ;t