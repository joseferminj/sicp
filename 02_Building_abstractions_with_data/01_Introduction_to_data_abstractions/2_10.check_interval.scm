;; Exercise 2.10.  Ben Bitdiddle, an expert systems programmer, looks
;; over Alyssa's shoulder and comments that it is not clear what it
;; means to divide by an interval that spans zero. Modify Alyssa's
;; code to check for this condition and to signal an error if it
;; occurs.

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
(define (div-interval x y)
  (define (span-zero? x)
    (and (<= (lower-bound x) 0)
         (>= (upper-bound x) 0)))
  (if (span-zero? y)
      (error "Error: the denominator spans 0")
      (mul-interval x 
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

