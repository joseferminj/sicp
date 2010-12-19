;; *Exercise 1.38:* In 1737, the Swiss mathematician Leonhard Euler
;; published a memoir `De Fractionibus Continuis', which included a
;; continued fraction expansion for e - 2, where e is the base of the
;; natural logarithms.  In this fraction, the n_i are all 1, and the
;; D_i are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, ....  Write
;; a program that uses your `cont-frac' procedure from *Note Exercise
;; 1-37:: to approximate e, based on Euler's expansion.

(define (cont-frac n d k)
  (define (recur i)
    (if (= k i)
        0
        (/ (n i) (+ (d i) (recur (+ i 1))))))
  (recur 1))

(define (d k)
  (define (term? t)
    (= 0 (remainder (- t 2) 3)))
  (if (term? k)
      (/ (+ (* 2 k) 2) 3)
      1))

(define (e-aprox k)
  (cont-frac (lambda (i) 1.0)
             d
             k))

