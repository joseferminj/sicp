;; Exercise 2.59.  Implement the union-set operation for the
;; unordered-list representation of sets.

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else (union-set (adjoin-set (car s2) s1) (cdr s2)))))



