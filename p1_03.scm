(define (squaresum x y)
  (+ (* x x) (* y y)))

(define (>= x y) (not (< x y)))

(define (squaresummax x y z) 
  (cond
   ((and (>= x z) (>= y z)) (squaresum x y))
   ((and (>= x y) (>= z y)) (squaresum x z))
   ((and (>= y x) (>= z x)) (squaresum y z))
   )
  )

(squaresummax 3 4 5)