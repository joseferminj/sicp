;; *Exercise 2.2:* Consider the problem of representing line segments
;; in a plane.  Each segment is represented as a pair of points: a
;; starting point and an ending point.  Define a constructor
;; `make-segment' and selectors `start-segment' and `end-segment'
;; that define the representation of segments in terms of points.
;; Furthermore, a point can be represented as a pair of numbers: the
;; x coordinate and the y coordinate.  Accordingly, specify a
;; constructor `make-point' and selectors `x-point' and `y-point'
;; that define this representation.  Finally, using your selectors
;; and constructors, define a procedure `midpoint-segment' that takes
;; a line segment as argument and returns its midpoint (the point
;; whose coordinates are the average of the coordinates of the
;; endpoints).  To try your procedures, you'll need a way to print
;; points:

;;      (define (print-point p)
;;        (newline)
;;        (display "(")
;;        (display (x-point p))
;;        (display ",")
;;        (display (y-point p))
;;        (display ")"))

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

;;; Calculates the midpoint of a given segment
(define (midpoint-segment s)
  (define (avg a b)
    (/ (+ a b) 2))
  (define (mid-point p1 p2)
    (make-point (avg (x-point p1) (x-point p2))
                (avg (y-point p1) (y-point p2))))
  (mid-point (start-segment s) (end-segment s)))
