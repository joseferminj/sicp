;; *Exercise 1.18:* Using the results of *Note Exercise 1-16:: and
;; *Note Exercise 1-17::, devise a procedure that generates an
;; iterative process for multiplying two integers in terms of adding,
;; doubling, and halving and uses a logarithmic number of steps.(4)

(define (double x)
  (* 2 x))

(define (halve x)
  (/ x 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-multiply2 a b)
  (fast-multiply-iter a b 0))

(define (fast-multiply-iter a b c)
  (cond ((= b 0) c)
        ((even? b) (fast-multiply-iter (double a) (halve b) c))
        (else (fast-multiply-iter a (- b 1) (+ a c)))))
;; Unit testing
;; ============
;;
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(define fast-multiply-tests
  (test-suite
   "Fast multiplication. 1.17 sicp exercise"
   (check-equal? (fast-multiply2 2 0) 0)
   (check-equal? (fast-multiply2 2 1) 2)
   (check-equal? (fast-multiply2 2 3) 6)
   (check-equal? (fast-multiply2 2 5) 10)
   (check-equal? (fast-multiply2 3 6) 18)))

