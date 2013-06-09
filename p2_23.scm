#lang racket

(define (for-each proc items)
  (if (null? items)
      true
      (and (proc (car items)) 
           (for-each proc (cdr items))))) ;If two seperate things need to be done in a follow-up way, is there a better way to do that?
                                          ;I try (and a b) -- as the returned values doesn't matter, but it still seems weird.
                                          ;In addition, I find out that "display" does not count as an argument
                                          ;(e.g., "(define xx (display xx) xx)", although "define" can only have two arguments),
                                          ;then how can I "display" something only when some "if" is true?

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))