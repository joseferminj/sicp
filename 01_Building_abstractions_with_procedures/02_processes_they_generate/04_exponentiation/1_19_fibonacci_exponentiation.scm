;; *Exercise 1.19:* There is a clever algorithm for computing the
;; Fibonacci numbers in a logarithmic number of steps.  Recall the
;; transformation of the state variables a and b in the `fib-iter'
;; process of section *Note 1-2-2::: a <- a + b and b <- a.  Call
;; this transformation T, and observe that applying T over and over
;; again n times, starting with 1 and 0, produces the pair _Fib_(n +
;; 1) and _Fib_(n).  In other words, the Fibonacci numbers are
;; produced by applying T^n, the nth power of the transformation T,
;; starting with the pair (1,0).  Now consider T to be the special
;; case of p = 0 and q = 1 in a family of transformations T_(pq),
;; where T_(pq) transforms the pair (a,b) according to a <- bq + aq +
;; ap and b <- bp + aq.  Show that if we apply such a transformation
;; T_(pq) twice, the effect is the same as using a single
;; transformation T_(p'q') of the same form, and compute p' and q' in
;; terms of p and q.  This gives us an explicit way to square these
;; transformations, and thus we can compute T^n using successive
;; squaring, as in the `fast-expt' procedure.  Put this all together
;; to complete the following procedure, which runs in a logarithmic
;; number of steps:(5)

;;      (define (fib n)
;;        (fib-iter 1 0 0 1 n))

;;      (define (fib-iter a b p q count)
;;        (cond ((= count 0) b)
;;              ((even? count)
;;               (fib-iter a
;;                         b
;;                         <??>      ; compute p'
;;                         <??>      ; compute q'
;;                         (/ count 2)))
;;              (else (fib-iter (+ (* b q) (* a q) (* a p))
;;                              (+ (* b p) (* a q))
;;                              p
;;                              q
;;                              (- count 1)))))

;; Solution
;; ========
;;
;; After applying the tranformation twice and rearranging the terms, then
;; we get the expression
;;
;;      b(qˆ2 + 2pq) + a(qˆ2 + 2pq) + a(pˆ2 + qˆ2)
;;
;; As result, we compute p' and q' in terms of p and q
;;
;;      q' = q^2 + 2pq
;;      p' = p^2 + q^2

(define (square n)
  (* n n))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (square q) (* 2 p q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1))))) 


;; Unit testing
;; ============
;;
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(define fibonacci-exponentiation-tests
  (test-suite
   "Fibonacci exponentiation tests. 1.19 sicp exercise"
   (check-equal? (fib 0) 0)
   (check-equal? (fib 1) 1)
   (check-equal? (fib 2) 1)
   (check-equal? (fib 3) 2)
   (check-equal? (fib 4) 3)))
