#lang racket
;; Exercise 2.50.  Define the transformation flip-horiz, which flips painters horizontally, 
;; and transformations that rotate painters counterclockwise by 180 degrees and 270 degrees.
(require (planet soegaard/sicp:2:1/sicp)) 

(define (my-flip-horiz painter)
  ((transform-painter 
                     (make-vect 1.0 0.0)   ; new origin
                     (make-vect 0.0 0.0)   ; new end of edge1
                     (make-vect 1.0 1.0)) painter)) ; new end of edge2

(define (my-rotate180 painter)
   ((transform-painter 
                     (make-vect 1.0 1.0)   ; new origin
                     (make-vect 0.0 1.0)   ; new end of edge1
                     (make-vect 1.0 0.0)) painter))
(define (my-rotate270 painter)
   ((transform-painter 
                     (make-vect 1.0 0)   ; new origin
                     (make-vect 1.0 1.0)   ; new end of edge1
                     (make-vect 0.0 0.0)) painter))
(paint einstein)
(paint (flip-horiz einstein))
(paint (my-flip-horiz einstein))
(paint (rotate180 einstein))
(paint (my-rotate180 einstein))
(paint (my-rotate270 einstein))
