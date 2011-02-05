;; Exercise 2.38.  The accumulate procedure is also known as
;; fold-right, because it combines the first element of the sequence
;; with the result of combining all the elements to the right. There
;; is also a fold-left, which is similar to fold-right, except that it
;; combines elements working in the opposite direction:

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;; What are the values of
(define fold-right foldr)
(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list '() (list 1 2 3))
(fold-left list '() (list 1 2 3))

;; Give a property that op should satisfy to guarantee that fold-right
;; and fold-left will produce the same values for any sequence.

;;; The op must satisfy the commutative property. For example, the
;;; addition and multiplication are commutative operation
(fold-right + 0 (list 1 2 3))
;;; 6
(fold-left  + 0 (list 1 2 3))
;;; 6
(fold-right * 1 (list 1 2 3))
;;; 6
(fold-left * 1 (list 1 2 3))
;;; 6

;;; but the substraction and the division are not commutative operations
(fold-right - 0 (list 1 2 3))
;;; 2
(fold-left - 0 (list 1 2 3))
;;; -6
(fold-right / 1 (list 1 2 3))
;;; 3/2
(fold-left / 1 (list 1 2 3))
;;; 1/6


