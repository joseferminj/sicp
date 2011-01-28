;; Exercise 2.14.  Demonstrate that Lem is right. Investigate the
;; behavior of the system on a variety of arithmetic expressions.Make
;; some intervals A and B,  and use them in computing the expressions
;; A/A and A/B.  You will get the most insight by using intervals
;; whose width is a  small percentage of the center value. Examine the
;; results of the  computation in center-percent form (see exercise
;; 2.12).

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

;;; Interval operations
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
(define (div-interval x y)
  (define (span-zero? x)
    (and (<= (lower-bound x) 0)
         (>= (upper-bound x) 0)))
  (if (span-zero? y)
      (error "Error: the denominator spans 0")
      (mul-interval x 
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;;; Parallel-resistor formula 1
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

;;; Parallel-resistor formula 2
(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
;;; Solution
(define a (make-center-percent 10 1))
(define b (make-center-percent 5 2))

(define a-div-a (div-interval a a))
(define a-div-b (div-interval a b))

;;; We can see in the following examples, the results are
;;; aproximations not exact values. The most remarkable case is when
;;; dividing a interval by itself, the center is a aproximation to 1
(center a-div-a)
;; 1.0002000200020003

(center a-div-b)
;; 2.001200480192077

(percent a-div-a)
;; 1.9998000199979908

(percent a-div-b)
;; 2.9994001199759954

;;; With the previous results, the two procedures for parallel-resitor
;;; will deliver different results

(par1 a b)
;; '(3.191447368421053 . 3.4804054054054046)

(par2 a b)
;; '(3.277702702702703 . 3.3888157894736834)
