;; Exercise 2.31.  Abstract your answer to exercise 2.30 to produce a
;; procedure tree-map with the property that square-tree could be
;; defined as

;;; Solution
;;; ========

(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))

(define (square x)
  (* x x))
(define (square-tree tree) (tree-map square tree))
