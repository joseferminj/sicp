;; Exercise 2.13.  Show that under the assumption of small percentage
;; tolerances there is a simple formula for the approximate percentage
;; tolerance of the product of two intervals in terms of the
;; tolerances of the factors. You may simplify the problem by assuming
;; that all numbers are positive.

;;; Constructor and accesor methods
(define (make-interval a b) (cons a b))
(define (lower-bound i)(car i))
(define (upper-bound i)(cdr i))

(define (make-center-percent c p)
  (let ([witdh (* c (/ p 100.0))])
        (make-interval (- c witdh) (+ c witdh))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
  (define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
  (* 100.0 (/ (width i) (center i))))


;;; Solution
;;; After trying the multiplication of several intervals, it seems
;;; that the percentage of the product of two intervalas can be
;;; approximated by the sum of the percentage of the factors

(define a (make-center-percent 5 1))
(define b (make-center-percent 2 1))
(percent (mul-interval a b))
;;; => 1.9998000199980017
(define c (make-center-percent 5 2))
(percent (mul-interval b c))
;; => 2.9994001199760034
(define e (make-center-percent 4 1.5))
(percent (mul-interval c e))
;; => 3.4989503149055157
