;; Exercise 2.40.  Define a procedure unique-pairs that, given an
;; integer n, generates the sequence of pairs (i,j) with 1< j< i<
;; n. Use unique-pairs to simplify the definition of prime-sum-pairs
;; given above.

;;; In Racket does not have `flapmap` procedure, but we can combine
;;; `foldr` `append` and `map` as following

(define (flatmap op seq)
  (foldr append '() (map op seq)))

(define (enumerate-interval low end)
  (if (> low end)
      '()
      (cons low (enumerate-interval (+ low 1) end))))

;;; Solution
;;; ========
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(unique-pairs 5)
;; '((2 1) (3 1) (3 2) (4 1) (4 2) (4 3) (5 1) (5 2) (5 3) (5 4))

;;; Simplifying `prime-sum-pairs`

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime? x) 
   (define (test divisor) 
     (cond ((> (* divisor divisor) x) true) 
           ((= 0 (remainder x divisor)) false) 
           (else (test (+ divisor 1))))) 
   (test 2)) 

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))
