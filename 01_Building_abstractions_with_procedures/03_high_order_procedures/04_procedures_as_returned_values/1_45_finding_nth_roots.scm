;; *Exercise 1.45:* We saw in section *Note 1-3-3:: that attempting
;; to compute square roots by naively finding a fixed point of y |->
;; x/y does not converge, and that this can be fixed by average
;; damping.  The same method works for finding cube roots as fixed
;; points of the average-damped y |-> x/y^2.  Unfortunately, the
;; process does not work for fourth roots--a single average damp is
;; not enough to make a fixed-point search for y |-> x/y^3 converge.
;; On the other hand, if we average damp twice (i.e., use the average
;; damp of the average damp of y |-> x/y^3) the fixed-point search
;; does converge.  Do some experiments to determine how many average
;; damps are required to compute nth roots as a fixed-point search
;; based upon repeated average damping of y |-> x/y^(n-1).  Use this
;; to implement a simple procedure for computing nth roots using
;; `fixed-point', `average-damp', and the `repeated' procedure of
;; *Note Exercise 1-43::.  Assume that any arithmetic operations you
;; need are available as primitives.

;;; Previous defined functions
(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f times)
  (cond ((= times 1) f)
        (else (compose f (repeated f (- times 1))))))

(define tolerance 0.00001)

(define (average-damp f)
  (define (average a b)
    (/ (+ a b) 2))
  (lambda (x) (average x (f x))))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (expt y 2))))
               1.0))

;;; Solution. After testing with several nth-roots, the number of average damps
;;; required is the (round (sqrt n))

(define (nth-roots n)
  (define (repeated-avg-damp f)
    ((repeated average-damp (round (sqrt n))) f))
  (lambda (x)
    (fixed-point (repeated-avg-damp (lambda (y) (/ x (expt y (- n 1)))))
                 1.0)))


