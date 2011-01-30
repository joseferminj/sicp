;; Exercise 2.22.  Louis Reasoner tries to rewrite the first
;; square-list procedure of exercise 2.21 so that it evolves an
;; iterative process:

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items '()))

;; Unfortunately, defining square-list this way produces the answer
;; list in the reverse order of the one desired. Why?

;; Louis then tries to fix his bug by interchanging the arguments to cons:

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items '()))

;; This doesn't work either. Explain.

;;; Solution
;;; ========

;;; The first iterative implementation `cons` operation add each
;;; square element to the begining of the list, so the produced list
;;; is in reverse order

;;; In the second iterative implementation, interchanging the
;;; arguments to cons does no resolve the problem, because it creates
;;; nested pairs
