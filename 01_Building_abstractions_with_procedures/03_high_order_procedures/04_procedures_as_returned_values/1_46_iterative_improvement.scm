;; *Exercise 1.46:* Several of the numerical methods described in
;; this chapter are instances of an extremely general computational
;; strategy known as "iterative improvement".  Iterative improvement
;; says that, to compute something, we start with an initial guess
;; for the answer, test if the guess is good enough, and otherwise
;; improve the guess and continue the process using the improved
;; guess as the new guess.  Write a procedure `iterative-improve'
;; that takes two procedures as arguments: a method for telling
;; whether a guess is good enough and a method for improving a guess.
;; `Iterative-improve' should return as its value a procedure that
;; takes a guess as argument and keeps improving the guess until it
;; is good enough.  Rewrite the `sqrt' procedure of section *Note
;; 1-1-7:: and the `fixed-point' procedure of section *Note 1-3-3::
;; in terms of `iterative-improve'.

;;; Solution
(define (iterative-improve good-enough? transform)
  (define (try guess)
    (let ((next (transform guess)))
      (if (good-enough? guess next)
          next
          (try next))))
  (lambda (x)
    (try x)))

;;; Implements sqrt using iterative-improve procedure
(define (sqrt x)
  (define (average a b)
    (/(+ a b) 2))
  (define (good-enough? guess next)
    (< (abs (- (expt guess 2) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

(sqrt 4)                                ;=> 2.000000000000002

;;; Implements fixed-point using iterative-improve procedure
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  ((iterative-improve close-enough? f) first-guess))

(fixed-point cos 1.0)                   ;=> 0.7390822985224023
