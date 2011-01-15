;; *Exercise 2.3:* Implement a representation for rectangles in a
;; plane.  (Hint: You may want to make use of *Note Exercise 2-2::.)
;; In terms of your constructors and selectors, create procedures
;; that compute the perimeter and the area of a given rectangle.  Now
;; implement a different representation for rectangles.  Can you
;; design your system with suitable abstraction barriers, so that the
;; same perimeter and area procedures will work using either
;; representation?

;;; Point constructor and selectors
(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

;;; Segment constructor and selectors
(define (make-segment p-start p-end)
  (cons p-start p-end))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

;;; Rectangle constructor and selector

;;; The rectangle is stored a pair of segments that represents the
;;; diagonals of the rectangle

(define (make-rectangle v1 v2 v3 v4)
  (cons (make-segment v1 v3)
        (make-segment v2 v4)))

(define (v1 r)
  (start-segment (car r)))

(define (v2 r)
  (start-segment (cdr r)))

(define (v3 r)
  (end-segment (car r)))

(define (v4 r)
  (end-segment (cdr r)))

(define (distance p1 p2)
  (sqrt (+ (expt (- (x-point p2) (x-point p1)) 2)
           (expt (- (y-point p2) (y-point p1)) 2))))

(define (height r)
  (distance (v1 r) (v2 r)))

(define (widht r)
  (distance (v2 r) (v4 r)))

;;; Rectangle operations
(define (per-rectangle r)
  (+ (* 2 (height r)) (* 2 (widht r))))

(define (area-rectangle r)
  (* (height r) (widht r)))

;;; Another way of implement a rectangle in a plane that keep the
;;; contract with the per-rectangle and area-rectangle methods but
;;; more incomplete
(define (make-rectangle v1 v2 v3 v4)
  (cons (distance v1 v2)
        (distance v2 v4)))

(define (height r)
  (car r))

(define (widht r)
  (cdr r))

