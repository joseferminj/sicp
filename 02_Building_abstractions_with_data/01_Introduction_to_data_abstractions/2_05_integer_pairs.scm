;; *Exercise 2.5:* Show that we can represent pairs of nonnegative
;; integers using only numbers and arithmetic operations if we
;; represent the pair a and b as the integer that is the product 2^a
;; 3^b.  Give the corresponding definitions of the procedures `cons',
;; `car', and `cdr'.
;;; Previous defined function to calculate the cube root

(define (cons-integer a b)
  (* (expt 2 a) (expt 3 b)))

(define (car-integer p)
  (if (= (modulo p 2) 0)
      (+ 1 (car-integer (/ p 2)))
      0))

(define (cdr-integer p)
  (if (= (modulo p 3) 0)
      (+ 1 (cdr-integer (/ p 3)))
      0))


