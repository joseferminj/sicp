;; Exercise 2.33.  Fill in the missing expressions to complete the following definitions of some basic list-manipulation operations as accumulations:

(define (map p sequence)
  (foldr (lambda (x y) (cons (p x) y)) '() sequence))

(define (append seq1 seq2)
  (foldl cons seq1 seq2))

(define (length sequence)
  (foldl (lambda (x y) (+ y 1)) 0 sequence))
