;; *Exercise 1.12:* The following pattern of numbers is called "Pascal's
;; triangle".

;;              1
;;            1   1
;;          1   2   1
;;        1   3   3   1
;;      1   4   6   4   1

;; The numbers at the edge of the triangle are all 1, and each number
;; inside the triangle is the sum of the two numbers above it.(4)
;; Write a procedure that computes elements of Pascal's triangle by
;; means of a recursive process.

;; Tree recursive solution
(define (pascal x y)
  (cond ((= x 0) 1)
        ((= x y) 1)
        (else (+ (pascal (- x 1) (- y 1)) (pascal x (- y 1))))))

;; Unit testing using SchemeUnit
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(define pascal-tests
  (test-suite
   "Pascal's triangle tests for 1.12 sicp exercise"
   (check-equal? (pascal 0 0) 1 "First element")
   (check-equal? (pascal 0 1) 1 "Second element")
   (check-equal? (pascal 1 1) 1 "Third element")
   (check-equal? (pascal 1 2) 2 "1,2 element")))

(run-tests pascal-tests)

