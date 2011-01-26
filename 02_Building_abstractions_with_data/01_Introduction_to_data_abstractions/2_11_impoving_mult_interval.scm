;; Exercise 2.11.  In passing, Ben also cryptically comments:``By
;; testing the signs of the endpoints of the intervals, it is possible
;; to break mul-interval into nine cases, only one of which requires
;; more  than two multiplications.'' Rewrite this procedure using
;; Ben's  suggestion.

;;; Constructor and accesor methods
(define (make-interval a b) (cons a b))
(define (lower-bound i)(car i))
(define (upper-bound i)(cdr i))

;;; Solution
;; The nine cases are:
;; [+, +] * [+, +]
;; [+, +] * [-, +]
;; [+, +] * [-, -]

;; [-, +] * [+, +]
;; [-, +] * [-, +]
;; [-, +] * [-, -]

;; [-, -] * [+, +]
;; [-, -] * [-, +]
;; [-, -] * [-, -]

;; The only case where we need to do more than two multiplications is
;; in the case [-, +] * [-, +], since the product of the two lower
;; bounds could possibly be larger than the product of the two upper bounds.
(define (mul-interval x y)
   (let ((xlo (lower-bound x))
         (xhi (upper-bound x))
         (ylo (lower-bound y))
         (yhi (upper-bound y)))
   (cond ((and (>= xlo 0)
               (>= xhi 0)
               (>= ylo 0)
               (>= yhi 0))
          ; [+, +] * [+, +]
          (make-interval (* xlo ylo) (* xhi yhi)))
         ((and (>= xlo 0)
               (>= xhi 0)
               (<= ylo 0)
               (>= yhi 0))
          ; [+, +] * [-, +]
          (make-interval (* xhi ylo) (* xhi yhi)))
         ((and (>= xlo 0)
               (>= xhi 0)
               (<= ylo 0)
               (<= yhi 0))
          ; [+, +] * [-, -]
          (make-interval (* xhi ylo) (* xlo yhi)))
         ((and (<= xlo 0)
               (>= xhi 0)
               (>= ylo 0)
               (>= yhi 0))
          ; [-, +] * [+, +]
          (make-interval (* xlo yhi) (* xhi yhi)))
         ((and (<= xlo 0)
               (>= xhi 0)
               (<= ylo 0)
               (>= yhi 0))
          ; [-, +] * [-, +]
          (make-interval (min (* xhi ylo) (* xlo yhi))
                         (max (* xlo ylo) (* xhi yhi))))
         ((and (<= xlo 0)
               (>= xhi 0)
               (<= ylo 0)
               (<= yhi 0))
          ; [-, +] * [-, -]
          (make-interval (* xhi ylo) (* xlo ylo)))
         ((and (<= xlo 0)
               (<= xhi 0)
               (>= ylo 0)
               (>= yhi 0))
          ; [-, -] * [+, +]
          (make-interval (* xlo yhi) (* xhi ylo)))
         ((and (<= xlo 0)
               (<= xhi 0)
               (<= ylo 0)
               (>= yhi 0))
          ; [-, -] * [-, +]
          (make-interval (* xlo yhi) (* xlo ylo)))
         ((and (<= xlo 0)
               (<= xhi 0)
               (<= ylo 0)
               (<= yhi 0))
          ; [-, -] * [-, -]
          (make-interval (* xhi yhi) (* xlo ylo))))))

(define a (make-interval 2 4))
(define b (make-interval -2 4))
(define c (make-interval -4 -2))
(mul-interval a a)
(mul-interval a b)
(mul-interval a c)
(mul-interval b a)
(mul-interval b b)
(mul-interval b c)
(mul-interval c a)
(mul-interval c b)
(mul-interval c c)
