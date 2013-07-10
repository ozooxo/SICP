#lang racket

((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
 10)
;==
((lambda (fact)
   (fact fact 10))
 (lambda (ft k)
   (if (= k 1)
       1
       (* k (ft ft (- k 1))))))
;==
((lambda (ft k)
   (if (= k 1)
       1
       (* k (ft ft (- k 1)))))
 (lambda (ft k)
   (if (= k 1)
       1
       (* k (ft ft (- k 1)))))
 10)
;((lambda (ft k) #a) #b 1) == 1
;((lambda (ft k) #a) #b 2) == 2*(#b #b 1) -- and it turns out that #b is also a (lambda (ft k) #a), so (#b #b 2) == 1.
;((lambda (ft k) #a) #b 3) == 3*(#b #b 2) == 3 * 2 * 1 == 6 ...
;Thus the function does calculate the factorial.

((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (cond ((= k 1) 1)
            ((= k 2) 1)
            (else (+ (ft ft (- k 1)) (ft ft (- k 2))))))))
 8)
;21 -- It does calculate the Fibonacci numbers.