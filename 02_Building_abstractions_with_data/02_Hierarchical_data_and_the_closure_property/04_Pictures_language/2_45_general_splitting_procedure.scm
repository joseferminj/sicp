#lang racket
(require (planet soegaard/sicp:2:1/sicp)) 

;;; Exercise 2.45.  Right-split and up-split can be expressed as instances 
;;; of a general splitting operation. Define a procedure split with the property
;;; that evaluating

(define (split l r)
  (λ (painter n)
    (if (= n 0)
        painter
        (let ((smaller (right-split painter (- n 1))))
          (l painter (r smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))

(paint(right-split einstein 2))
(paint(up-split einstein 2))

; produces procedures right-split and up-split with the same behaviors as the ones already defined.