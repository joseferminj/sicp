#lang racket
;; Exercise 2.51.  Define the below operation for painters.
;; Below takes two painters as arguments. The resulting painter, 
;; given a frame, draws with the first painter in the bottom 
;; of the frame and with the second painter in the top. 
;; Define below in two different ways -- first by writing 
;; a procedure that is analogous to the beside procedure 
;; given above, and again in terms of beside and suitable 
;; rotation operations (from exercise 2.50).
(require (planet soegaard/sicp:2:1/sicp)) 

(define (my-below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-below
           (transform-painter 
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point))
          (paint-top
           (transform-painter 
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        ((paint-below painter1) frame)
        ((paint-top painter2) frame)))))

(define (below-with-beside p1 p2)
  (rotate90 (beside (rotate270 p1) (rotate270 p2))))

(define (shrink-to-upper-right painter)
  ((transform-painter 
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)) painter))


(paint (my-below einstein einstein))
(paint (shrink-to-upper-right einstein))
(paint (beside einstein einstein))
(paint (below-with-beside einstein einstein))