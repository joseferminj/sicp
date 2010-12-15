;; *Exercise 1.29:* Simpson's Rule is a more accurate method of
;; numerical integration than the method illustrated above.  Using
;; Simpson's Rule, the integral of a function f between a and b is
;; approximated as

;;      h
;;      - (y_0 + 4y_1 + 2y_2 + 4y_3 + 2y_4 + ... + 2y_(n-2) + 4y_(n-1) + y_n)
;;      3

;; where h = (b - a)/n, for some even integer n, and y_k = f(a + kh).
;; (Increasing n increases the accuracy of the approximation.)
;; Define a procedure that takes as arguments f, a, b, and n and
;; returns the value of the integral, computed using Simpson's Rule.
;; Use your procedure to integrate `cube' between 0 and 1 (with n =
;; 100 and n = 1000), and compare the results to those of the
;; `integral' procedure shown above.


(define (h a b n)
    (/ (- b a) n))

(define (y-term f a h k n)
  (define (y f a k h)
    (f (+ a (* k h))))
  (cond ((= k 0) (y f a k h))
        ((= k n) (y f a k h))
        ((even? k) (* 2 (y f a k h)))
        (else (* 4 (y f a k h)))))

(define (y_serie f a h k n)
  (cond ((= k 0) (y-term f a h k n))
        (else (+ (y-term f a h k n) (y_serie f a h (- k 1) n)))))

(define (simpson-method f a b n)
  (* (/ (h a b n) 3) (y_serie f a (h a b n) n n)))

;; Unit testing
;; ============
;;
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(define (cube x)
  (* x x x))

(define simpson-method-tests
  (test-suite
   "Simpson's method for numerical integration  tests. 1.29 sicp exercise"
   (check-equal? (simpson-method cube 0 1 100) (/ 1 4))
   (check-equal? (simpson-method cube 0 1 1000) (/ 1 4))))
