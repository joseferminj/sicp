;; *Exercise 1.36:* Modify `fixed-point' so that it prints the
;; sequence of approximations it generates, using the `newline' and
;; `display' primitives shown in *Note Exercise 1-22::.  Then find a
;; solution to x^x = 1000 by finding a fixed point of x |->
;; `log'(1000)/`log'(x).  (Use Scheme's primitive `log' procedure,
;; which computes natural logarithms.)  Compare the number of steps
;; this takes with and without average damping.  (Note that you
;; cannot start `fixed-point' with a guess of 1, as this would cause
;; division by `log'(1) = 0.)


;;; Implementing `fixed-point` with average damping
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display next)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;;; Solution
;;; ========

(define (average a b)
  (/ (+ a b) 2))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2)
;;; Without average dumping, it takes 34 steps to reach the fixed point
;; 9.965784284662087
;; 3.004472209841214
;; 6.279195757507157
;; 3.7598507024015393
;; 5.2158437849258945
;; 4.182207192401398
;; 4.82776509834459
;; 4.387593384662677
;; 4.671250085763899
;; 4.481403616895052
;; 4.6053657460929
;; 4.5230849678718865
;; 4.577114682047341
;; 4.541382480151454
;; 4.564903245230833
;; 4.549372679303342
;; 4.559606491913287
;; 4.552853875788271
;; 4.557305529748263
;; 4.554369064436181
;; 4.556305311532999
;; 4.555028263573554
;; 4.555870396702851
;; 4.555315001192079
;; 4.5556812635433275
;; 4.555439715736846
;; 4.555599009998291
;; 4.555493957531389
;; 4.555563237292884
;; 4.555517548417651
;; 4.555547679306398
;; 4.555527808516254
;; 4.555540912917957
;; 4.5555322708036534.555532270803653

(fixed-point (lambda (x) (average x  (/ (log 1000) (log x)))) 2)
;;; With average dumping, it takes 9 steps to reach the fixed point
;; 5.9828921423310435
;; 4.922168721308343
;; 4.628224318195455
;; 4.568346513136243
;; 4.5577305909237005
;; 4.555909809045131
;; 4.555599411610624
;; 4.5555465521473675
;; 4.5555375519998254.555537551999825
