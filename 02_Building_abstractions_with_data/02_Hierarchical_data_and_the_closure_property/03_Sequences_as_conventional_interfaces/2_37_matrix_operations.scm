;; Exercise 2.37.  Suppose we represent vectors v = (vi) as sequences
;; of numbers, and matrices m = (mij) as sequences of vectors (the
;; rows of the matrix). For example, thematrix

;; is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8
;; 9)). With this representation, we can use sequence operations to
;; concisely express the basic matrix and vector operations. These
;; operations (which are described in any book on matrix algebra) are
;; the following:

;; We can define the dot product as:

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;; Fill in the missing expressions in the following procedures for
;; computing the other matrix operations. (The procedure accumulate-n
;; is defined in exercise2.36.)
(define accumulate foldr)

(define (matrix-*-vector m v)
  (define (vector-product w)
    (dot-product v w))
   (map vector-product m))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (define (cols-*-vector v)
      (matrix-*-vector cols v))
    (map cols-*-vector m)))
