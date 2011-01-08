;; *Exercise 2.1:* Define a better version of `make-rat' that handles
;; both positive and negative arguments.  `Make-rat' should normalize
;; the sign so that if the rational number is positive, both the
;; numerator and denominator are positive, and if the rational number
;; is negative, only the numerator is negative.

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;;; Solution
;;; ========

(define (make-rat n d)
  (define (make n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (cond((< d 0) (make (* n -1) (* d -1)))
        (else (make n d))))

;;; Testing
;;; =======
(print-rat (make-rat -2 -3))   ; => 2/3

(print-rat (make-rat 2 -3))  ; => -2/3

(print-rat (make-rat -2 3)) ; => -2/3

(print-rat (make-rat 2 3))               ; => 2/3
