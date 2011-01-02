;; *Exercise 1.39:* A continued fraction representation of the
;; tangent function was published in 1770 by the German mathematician
;; J.H. Lambert:

;;                    x
;;      tan x = ---------------
;;                      x^2
;;              1 - -----------
;;                        x^2
;;                  3 - -------
;;                      5 - ...

;; where x is in radians.  Define a procedure `(tan-cf x k)' that
;; computes an approximation to the tangent function based on
;; Lambert's formula.  `K' specifies the number of terms to compute,
;; as in *Note Exercise 1-37::.

;;; From previos exersices we've already define the cont-frac function
(define (cont-frac n d k)
  (define (recur i)
    (if (= k i)
        0
        (/ (n i) (+ (d i) (recur (+ i 1))))))
  (recur 1))


(define (tan-cf x k)
  (define (n k)
    (if (= k 1)
        x
        (* x x)))
  (define (d k)
    (- (* 2 k) 1))
  (cont-frac n d k))
