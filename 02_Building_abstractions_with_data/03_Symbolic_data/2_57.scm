;; Exercise 2.57.  Extend the differentiation program to handle sums
;; and products of arbitrary numbers of (two or more) terms. Then the
;; last example above could be expressed as


;; (deriv '(* x y (+ x 3)) 'x)

;; Try to do this by changing only the representation for sums and
;; products, without changing the deriv procedure at all. For example,
;; the addend of a sum would be the first term, and the augend would
;; be the sum of the rest of the terms.


;;; Accesors
(define (=number? a b)
  (and (number? a) (number? b) (= a b)))
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s)
  (make-sum-list (cddr s)))

(define (make-sum-list seq)
  (if (null? seq)
      0
      (make-sum (car seq) (make-sum-list (cdr seq)))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (make-product-list (cddr p)))

(define (make-product-list seq)
  (if (null? seq)
      1
      (make-product (car seq) (make-product-list (cdr seq)))))

;;; Accesors and constructor for new clause
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) 'expt)))

(define (base x) (cadr x))
(define (exponent x) (caddr x))

(define (make-exponentiation b e)
  (cond [(=number? e 0) 1]
        [(=number? e 1) e]
        [(and (number? e) (number? b) (expt b e))]
        [else (list 'expt b e)]))

;;; Basic differentiator

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (let* ([b (base exp)]
                [e (exponent exp)])
           (make-product (make-product e
                                       (make-exponentiation b
                                                            (make-sum e -1)))
                         (deriv b var))
           ))
        (else
         (error "unknown expression type -- DERIV" exp))))

