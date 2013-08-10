#lang racket

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define below-mine1
  (let ([split-point (make-vect 0.0 0.5)])
    (let ([paint-left
           (transform-painter (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point)]
          [paint-right
           (transform-painter split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))])
      (lambda (painter1 painter2)
        (superpose (paint-left painter1)
                   (paint-right painter2))))))
;Notice that soegaard's package defines (transform-painter origin corner1 corner2),
;while SICP defines (transform-painter painter origin corner1 corner2).
;So we do more than some minor changes from the "beside" given by SICP

(define (below-mine2 painter1 painter2)
  (rotate270 (beside (rotate90 painter1) (rotate90 painter2))))

(paint (below einstein einstein))
(paint (below-mine1 einstein einstein))
(paint (below-mine2 einstein einstein))

;So in some sense, "transform-painter" is doing something related to the relative coordinate
;(a.k.a., the frame), so some reductive things like
;(paint (below-mine1 (below-mine1 einstein einstein) (below-mine1 einstein einstein)))
;make sense. I don't quite understand in the deeper level how to do that,
;especially in soegaard's package, "frame" never goes as an explicit parameter.

;In addition, it seems in SICP that everything you kan "paint" them is a function of "frame".
;I do can see clearly that they are procedures (e.g., "einstein" is a procedure).
;However, I'm not quite sure how they helps.