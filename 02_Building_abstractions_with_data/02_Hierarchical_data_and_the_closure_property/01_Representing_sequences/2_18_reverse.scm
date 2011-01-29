;; Exercise 2.18.  Define a procedure reverse that takes a list as
;; argument and returns a list of the same elements in reverse order:

(reverse (list 1 4 9 16 25))
;; (25 16 9 4 1)

;;; Solution
;;; ========

(define (reverse l)
  (define (r l1 l2)
    (cond [(null? l1) l2]
          [else (r (cdr l1) (cons (car l1) l2))]))
  (r l '()))

;; Unit testing
;; ============
;;
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(define reverse-tests
  (test-suite
   "Reverse list procedure tests. 2.18 sicp exercise"
   (check-equal? (reverse (list 1 4 9 16 25)) (list 25 16 9 4 1))
   (check-equal? (reverse (list 1)) (list 1))
   (check-equal? (reverse (list)) (list))))
