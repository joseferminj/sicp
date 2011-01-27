;; Exercise 2.12.  Define a constructor make-center-percent that takes
;; a center and a percentage tolerance and produces the desired
;; interval. You must also define a selector percent that produces the
;; percentage tolerance for a given interval. The center selector is
;; the same as the one shown above.

;;; Constructor and accesor methods
(define (make-interval a b) (cons a b))
(define (lower-bound i)(car i))
(define (upper-bound i)(cdr i))

;;; Solution
(define (make-center-percent c p)
  (let ([witdh (* c (/ p 100.0))])
        (make-interval (- c witdh) (+ c witdh))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
  (define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
  (* 100.0 (/ (width i) (center i))))

(define c (make-interval 5 20))
;; => '(4.0 . 6.0)
(center x)
;; => 5.0
(percent x)
;;; => 20.0
