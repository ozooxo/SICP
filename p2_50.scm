#lang racket

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define flip-horiz-mine
  (transform-painter (make-vect 1 0)
                     (make-vect 0 0)
                     (make-vect 1 1)))
;Notice that soegaard's package defines (transform-painter origin corner1 corner2),
;while SICP defines (transform-painter painter origin corner1 corner2). Those two are not identical.

;(paint einstein)
(paint (flip-horiz einstein))
(paint (flip-horiz-mine einstein)) ;same as the above one.