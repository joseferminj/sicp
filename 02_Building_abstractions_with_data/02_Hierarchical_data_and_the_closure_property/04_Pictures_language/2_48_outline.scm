#lang racket
(require (planet soegaard/sicp:2:1/sicp)) 

(define one 0.99)
(define tr (make-vect one one))
(define tl (make-vect 0 one))
(define br (make-vect one 0))
(define bl (make-vect 0 0))

(define (outline f)
  (let* ([s1 (make-segment bl br)]
         [s2 (make-segment bl tl)]
         [s3 (make-segment br tr)]
         [s4 (make-segment tl tr)])
    ((segments->painter (list s1 s2 s3 s4)) f)))

(define (x f)
  (let* ([s1 (make-segment bl tr)]
         [s2 (make-segment br tl)])
    ((segments->painter (list s1 s2)) f)))

(define tm (make-vect 0.5 one))
(define rm (make-vect one 0.5))
(define bm (make-vect 0.5 0))
(define lm (make-vect 0 0.5))

(define (diamond f)
  (let* ([s1 (make-segment tm rm)]
         [s2 (make-segment rm bm)]
         [s3 (make-segment bm lm)]
         [s4 (make-segment lm tm)])
    ((segments->painter (list s1 s2 s3 s4)) f)))
(paint outline)
(paint x)
(paint diamond)
