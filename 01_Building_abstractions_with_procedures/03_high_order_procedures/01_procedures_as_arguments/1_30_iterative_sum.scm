;; *Exercise 1.30:* The `sum' procedure above generates a linear
;; recursion.  The procedure can be rewritten so that the sum is
;; performed iteratively.  Show how to do this by filling in the
;; missing expressions in the following definition:

;;      (define (sum term a next b)
;;        (define (iter a result)
;;          (if <??>
;;              <??>
;;              (iter <??> <??>)))
;;        (iter <??> <??>))

;;; Solution
;;; ========

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;; Unit testing
;; ============
;;
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(define (square x)
  (* x x))

(define (inc n)
  (+ n 1))

(define sum-iter-tests
  (test-suite
   "Iterative sum with a procedure as argument. 1.30 sicp exercise"
   (check-equal? (sum square 1 inc 2) 5)
   (check-equal? (sum square 1 inc 5) 55)))
