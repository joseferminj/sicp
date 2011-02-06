;; Exercise 2.41.  Write a procedure to find all ordered triples of
;; distinct positive integers i, j, and k less than or equal to a
;; given integer n that sum to a given integer s.

(define (flatmap op seq)
  (foldr append '() (map op seq)))

(define (enumerate-interval low end)
  (if (> low end)
      '()
      (cons low (enumerate-interval (+ low 1) end))))

(define (triples n)
  (flatmap (lambda (i) (flatmap (lambda (j)  (map (lambda (k) (list i j k))
                                    (enumerate-interval 1 n)))
                       (enumerate-interval 1 n)))
           (enumerate-interval 1 n)))

(define (ordered-triples n s)
  (filter (lambda (triple)
            (> s (+ (first triple) (second triple) (third triple))))
          (triples n)))
