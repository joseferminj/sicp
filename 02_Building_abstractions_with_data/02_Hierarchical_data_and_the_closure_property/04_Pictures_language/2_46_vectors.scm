;; Exercise 2.46.  A two-dimensional vector v running from the origin
;; to a point can be represented as a pair consisting of an
;; x-coordinate and a y-coordinate. Implement a data abstraction for
;; vectors by giving a constructor make-vect and corresponding
;; selectors xcor-vect and ycor-vect. In terms of your selectors and
;; constructor, implement procedures add-vect, sub-vect, and
;; scale-vect that perform the operations vector addition, vector
;; subtraction, and multiplyinga vector by a scalar:

(define (make-vector x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))


(define (add-vect v w)
  (make-vector (+ (xcor-vect v) (xcor-vect w))
               (+ (ycor-vect v) (ycor-vect w))))

(define (sub-vect v w)
  (make-vector (- (xcor-vect v) (xcor-vect w))
               (- (ycor-vect v) (ycor-vect w))))

(define (scala-vect s v)
  (make-vector (* s (xcor-vect v))
               (* s (ycor-vect w))))

(define v (make-vector 2 3))
(define w (make-vector 1 1))

(add-vect v w)
(sub-vect v w)
(scala-vect 5 v)
