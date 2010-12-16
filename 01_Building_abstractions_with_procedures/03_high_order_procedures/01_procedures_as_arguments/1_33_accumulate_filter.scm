;; *Exercise 1.33:* You can obtain an even more general version of
;; `accumulate' (*Note Exercise 1-32::) by introducing the notion of
;; a "filter" on the terms to be combined.  That is, combine only
;; those terms derived from values in the range that satisfy a
;; specified condition.  The resulting `filtered-accumulate'
;; abstraction takes the same arguments as accumulate, together with
;; an additional predicate of one argument that specifies the filter.
;; Write `filtered-accumulate' as a procedure.  Show how to express
;; the following using `filtered-accumulate':

;;   a. the sum of the squares of the prime numbers in the interval a
;;      to b (assuming that you have a `prime?' predicate already
;;      written)

;;   b. the product of all the positive integers less than n that are
;;      relatively prime to n (i.e., all positive integers i < n such
;;      that GCD(i,n) = 1).

;;; Solution
;;; ========
(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a acc)
    (cond ((> a b) acc)
          ((filter a) (iter (next a) (combiner (term a) acc)))
          (else (iter (next a) acc))))
  (iter a null-value))

;;; The sum of the squares of the prime numbers in the interval a to b
(require "../../02_processes_they_generate/06_Testing_for_Primality/1_22_search_for_primes.scm")

(define (square x)
  (* x x))
(define (inc x)
  (+ x 1))

(define (sum-square-primes a b)
  (filtered-accumulate prime? + 0 square a inc b))

;;; The product of all the positive integers less than n that are relatively
;;; prime to n
(define (identity x)
  x)

(define (product-relative-primes n)
  (define (relatively-prime? i)
    (define (gcd a b)
      (if (= b 0)
          a
          (gcd b (remainder a b))))
    (= (gcd i n) 1))

  (filtered-accumulate relatively-prime? * 1 identity 1 inc n))

;;; Unit testing
;;; ============

(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))
(define sum-square-tests
  (test-suite
   "Testing filtered-accumulate function. 1.33 sicp exercise"
   (check-equal? (sum-square-tests) 39)
   (check-equal? (product-relative-primes 5) 24)
   (check-equal? (product-relative-primes 6) 5)))
