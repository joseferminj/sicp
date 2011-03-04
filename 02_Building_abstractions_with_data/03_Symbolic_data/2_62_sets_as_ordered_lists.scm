;; Exercise 2.62.  Give a (n) implementation of union-set for sets
;; represented as ordered lists.

(define (adjoin-set x set)
  (cond [(null? set) (list x)]
        [(eq? x (car set)) set]
        [(< x (car set))(cons x set)]
        [else (cons (car set) (adjoin-set x (cdr set)))]))

(define (union-set s1 s2)
  (cond [(null? s1) s2]
        [(null? s2) s1]
        [(< (car s1) (car s2)) (cons (car s1) (union-set (cdr s1) s2))]
        [else (cons (car s2) (union-set s1 (cdr s2)))]))
