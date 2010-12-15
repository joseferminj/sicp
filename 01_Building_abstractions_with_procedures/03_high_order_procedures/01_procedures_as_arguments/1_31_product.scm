;; *Exercise 1.31:*
;;   a. The `sum' procedure is only the simplest of a vast number of
;;      similar abstractions that can be captured as higher-order
;;      procedures.(3)  Write an analogous procedure called `product'
;;      that returns the product of the values of a function at
;;      points over a given range.  Show how to define `factorial' in
;;      terms of `product'.  Also use `product' to compute
;;      approximations to [pi] using the formula(4)

;;           pi   2 * 4 * 4 * 6 * 6 * 8 ...
;;           -- = -------------------------
;;            4   3 * 3 * 5 * 5 * 7 * 7 ...

;;   b. If your `product' procedure generates a recursive process,
;;      write one that generates an iterative process.  If it
;;      generates an iterative process, write one that generates a
;;      recursive process.


(define (product term a b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (+ a 1) (* (term a) result))))
  (iter a 1))

(define (identity n)
  n)

(define (factorial n)
  (product identity 1 n))

;; In wikipedia http://en.wikipedia.org/wiki/Wallis_product
;; we can see the formula expressed as a product of series
(define (wallis-pi n)
   (define (term x)
      (/ (* 4.0 (square x))
         (- (* 4.0 (square x)) 1)))
   (* 2.0 (product term 1 n)))

;; Unit testing
;; ============
;;
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(define (square x)
  (* x x))


(define product-tests
  (test-suite
   "Iterative product of values of a function in a given range. 1.31 sicp exercise"
   (check-equal? (product square 1 2) 4)
   (check-equal? (product square 1 5) 14400)))

(define factorial-product-tests
  (test-suite
   "Factorial in terms of  product. 1.31 sicp exercise"
   (check-equal? (factorial 0) 1)
   (check-equal? (factorial 1) 1)
   (check-equal? (factorial 5) 120)))

