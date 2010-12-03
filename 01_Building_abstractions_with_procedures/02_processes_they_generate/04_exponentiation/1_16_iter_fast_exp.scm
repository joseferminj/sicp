;; *Exercise 1.16:* Design a procedure that evolves an iterative
;; exponentiation process that uses successive squaring and uses a
;; logarithmic number of steps, as does `fast-expt'.
;; (Hint: Using the observation that
;;
;;    (b^(n/2))^2 = (b^2)^(n/2)
;;
;; , keep, along with the
;; exponent n and the base b, an additional state variable a, and
;; define the state transformation in such a way that the product a
;; b^n is unchanged from state to state.  At the beginning of the
;; process a is taken to be 1, and the answer is given by the value
;; of a at the end of the process.
;;
;; In general, the technique of
;; defining an "invariant quantity" that remains unchanged from state
;; to state is a powerful way to think about the design of iterative
;; algorithms.)

;; Solution
;; ========
;;

(define (square n)
  (* n n))

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt2 b n)
  (fast-expt-iter b n 1))

(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter (square b) (/ n 2) a))
        (else (fast-expt-iter b (- n 1) (* b a)))))


;; Unit testing
;; ============
;;
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(define fast-expt-tests
  (test-suite
   "Iterative fast exponentiation. 1.16 sicp exercise"
   (check-equal? (fast-expt2 2 0) 1 )
   (check-equal? (fast-expt2 2 1) 2 )
   (check-equal? (fast-expt2 2 2) 4 )
   (check-equal? (fast-expt2 2 7) 128 )
   (check-equal? (fast-expt2 2 8) 256 )))
