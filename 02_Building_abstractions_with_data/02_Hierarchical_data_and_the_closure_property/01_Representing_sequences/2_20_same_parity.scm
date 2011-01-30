;;; Exercise 2.20. Use this notation to write a procedure same-parity
;;; that takes on e or more integers and returns a list of all the
;;; arguments that have the same even-odd parity as the first
;;; argument.

;;; Solution
;;; ========

(define (same-parity . l)
  (define (parity? x y)
    (= (modulo x 2) (modulo y 2)))

  (define (p x l)
    (cond [(null? l) (list x)]
          [(parity? x (car l)) (cons x (p (car l) (cdr l)))]
          [else (p x (cdr l))]))
  (p (car l) (cdr l)))

;; Unit testing
;; ============
;;
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(define parity-tests
  (test-suite
   "Same parity procedure. 2.20 sicp exercise"
   (check-equal? (same-parity 1 2 3 4 5 6 7) (list 1 3 5 7))
   (check-equal? (same-parity 2 3 4 5 6 7) (list 2 4 6))
   (check-equal? (same-parity 2) (list 2))))
