;; Exercise 2.35.  Redefine count-leaves from section 2.2.2 as an
;; accumulation:

(define accumulate foldr)

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) (if (pair? x)
                                  (count-leaves x)
                                  1)) t)))
