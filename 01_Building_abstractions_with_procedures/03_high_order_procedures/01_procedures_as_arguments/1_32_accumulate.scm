;; *Exercise 1.32:*
;;   a. Show that `sum' and `product' (*Note Exercise 1-31::) are
;;      both special cases of a still more general notion called
;;      `accumulate' that combines a collection of terms, using some
;;      general accumulation function:

;;           (accumulate combiner null-value term a next b)

;;      `Accumulate' takes as arguments the same term and range
;;      specifications as `sum' and `product', together with a
;;      `combiner' procedure (of two arguments) that specifies how
;;      the current term is to be combined with the accumulation of
;;      the preceding terms and a `null-value' that specifies what
;;      base value to use when the terms run out.  Write `accumulate'
;;      and show how `sum' and `product' can both be defined as
;;      simple calls to `accumulate'.

;;   b. If your `accumulate' procedure generates a recursive process,
;;      write one that generates an iterative process.  If it
;;      generates an iterative process, write one that generates a
;;      recursive process.

;;; Solution
;;; ========

;; Iterative solution
(define (accumulate combiner null-value term a next b)
  (define (iter a acc)
    (if (> a b)
        acc
        (iter (next a) (combiner (term a) acc))))
  (iter a null-value))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (sum term a next b)
  (accumulate + 0 term a next b))

;; Recursive solution
(define (accumulate-recur combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate-recur combiner null-value term (next  a) next b))))

(define (product-recur term a next b)
  (accumulate-recur * 1 term a next b))

;;; Unit testing
;;; ============

(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(define (square x)
  (* x x))

(define (inc x)
  (+ x 1))

(define product-square-tests
  (test-suite
   "Product of square using accumulate function. 1.32 sicp exercise"
   (check-equal? (product square 1 inc 2) 4)
   (check-equal? (product square 1 inc 5) 14400)))

(define sum-square-tests
  (test-suite
   "Sum of square using accumulate function. 1.32 sicp exercise"
   (check-equal? (sum square 1 inc 2) 5)
   (check-equal? (sum square 1 inc 5) 55)))

(define product-recur-square-tests
  (test-suite
   "Product of square using accumulate recursive function. 1.32 sicp exercise"
   (check-equal? (product-recur square 1 inc 2) 4)
   (check-equal? (product-recur square 1 inc 5) 14400)))
