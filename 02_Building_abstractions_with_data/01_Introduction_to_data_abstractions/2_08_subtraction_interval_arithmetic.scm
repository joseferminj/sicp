;; Exercise 2.8.  Using reasoning analogous to Alyssa's, describe how
;; the difference of two intervals may be computed. Define a
;; corresponding subtraction procedure, called sub-interval.

;;; Constructor and accesor methods
(define (make-interval a b) (cons a b))
(define (lower-bound i)(car i))
(define (upper-bound i)(cdr i))

;;;
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))



