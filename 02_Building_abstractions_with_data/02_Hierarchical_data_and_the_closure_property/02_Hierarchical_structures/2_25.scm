;; Exercise 2.25.  Give combinations of cars and cdrs that will pick 7
;; from each of the following lists:

;; (1 3 (5 7) 9)

(define l '(1 3 (5 7) 9))
(car (cdr (car (cdr (cdr l)))))
;; ((7))
(define l '((7)))
(car (car l))

;; (1 (2 (3 (4 (5 (6 7))))))
(define l '(1 (2 (3 (4 (5 (6 7)))))))
(define (car-cdr l)
  (car (cdr l)))
(car-cdr (car-cdr (car-cdr (car-cdr (car-cdr (car-cdr l))))))

