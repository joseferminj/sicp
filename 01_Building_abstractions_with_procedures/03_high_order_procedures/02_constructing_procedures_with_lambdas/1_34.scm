;; *Exercise 1.34:* Suppose we define the procedure

     (define (f g)
       (g 2))

;; Then we have

;;      (f square)
;;      4

;;      (f (lambda (z) (* z (+ z 1))))
;;      6

;; What happens if we (perversely) ask the interpreter to evaluate
;; the combination `(f f)'?  Explain.

;;; Solution
;;; ========
;;; The interpreter will raise a error because it will try to evaluate (2 2)
;;; when it expects a procedure instead of number
