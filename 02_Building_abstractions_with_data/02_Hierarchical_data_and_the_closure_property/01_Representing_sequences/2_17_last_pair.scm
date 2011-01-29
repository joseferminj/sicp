;; Exercise 2.17.  Define a procedure last-pair that returns the list
;; that contains only the  last element of a given (nonempty) list:

(last-pair (list 23 72 149 34))
;;; (34)

;; Solution
;; ========

(define (last-pair l)
  (cond [(null? l) (list)]
        [(null? (cdr l)) (list (car l))]
        [else (last-pair (cdr l))]))

;; Unit testing
;; ============
;;
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(define last-pair-tests
  (test-suite
   "Last pair procedure tests. 2.17 sicp exercise"
   (check-equal? (last-pair (list 23 72 149 34)) (list 34))
   (check-equal? (last-pair (list 34)) (list 34))
   (check-equal? (last-pair (list)) (list))))

