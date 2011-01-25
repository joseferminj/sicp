;; Exercise 2.9.  The width of an interval is half of the difference
;; between its upper and lower bounds. The width is a measure of the
;; uncertainty of the number specified by the interval. For some
;; arithmetic operations the width of the result of combining two
;; intervals is a function only of the widths of the argument
;; intervals, whereas for others the width of the combination is not a
;; function of the widths of the argument intervals. Show that the
;; width of the sum (or difference) of two intervals is a function
;; only of the widths of the intervals being added (or subtracted).
;; Give examples to show that this is not true for multiplication or division.

;;; Constructor and accesor methods
(define (make-interval a b) (cons a b))
(define (lower-bound i)(car i))
(define (upper-bound i)(cdr i))

;;; Operations
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))



;;; Solution
(define (width-interval x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

;; the width of the sum (or difference) of two intervals is a function
;; only of the widths of the intervals being added (or subtracted)
(width-interval (add-interval a b))
;; 7/2
(+ (width-interval a) (width-interval b))
;; 7/2
(width-interval (sub-interval a b))
;; 1/2
(- (width-interval a) (width-interval b))
;; 1/2

;;; The width of the multiplication is not
(width-interval (mul-interval a b))
;; 27/2
(* (width-interval a) (width-interval b))
;; 3
