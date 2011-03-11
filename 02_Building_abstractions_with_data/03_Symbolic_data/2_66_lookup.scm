;; Exercise 2.66.  Implement the lookup procedure for the case where
;; the set of records is structured as a binary tree, ordered by the
;; numerical values of the keys.

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (key entry)
  (car entry))
(define (value entry)
  (cdr entry))
(define (make-entry key value)
  (cons key value))

(define (lookup given-key s)
  (if (null? s)
      false
      (let ([k (key (entry s))]
            [v (value (entry s))])
        (cond [(= given-key k) v]
              [(< given-key k) (lookup given-key (left-branch s))]
              [(> given-key k) (lookup given-key (right-branch s))]))))

(define example  (make-tree (make-entry 7 "siete")
                            (make-tree (make-entry 3 "tres")
                                       (make-tree (make-entry 1 "uno")
                                                  '()
                                                  '())
                                       (make-tree (make-entry 5 "cinco")
                                                  '()
                                                  '()))
                            (make-tree (make-entry 9 "nueve")
                                       '()
                                       (make-tree (make-entry 11 "once")
                                                  '()
                                                  '()))))

(lookup 4 example)
;;; #f
(lookup 9 example)
;;; "nueve"
(lookup 5 example)
;;; "cinco"
