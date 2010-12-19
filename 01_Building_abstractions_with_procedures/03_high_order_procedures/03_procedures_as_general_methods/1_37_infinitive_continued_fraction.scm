;; *Exercise 1.37:*
;;   a. An infinite "continued fraction" is an expression of the form

;;                      N_1
;;           f = ---------------------
;;                          N_2
;;               D_1 + ---------------
;;                              N_3
;;                     D_2 + ---------
;;                           D_3 + ...

;;      As an example, one can show that the infinite continued
;;      fraction expansion with the n_i and the D_i all equal to 1
;;      produces 1/[phi], where [phi] is the golden ratio (described
;;      in section *Note 1-2-2::).  One way to approximate an
;;      infinite continued fraction is to truncate the expansion
;;      after a given number of terms.  Such a truncation--a
;;      so-called finite continued fraction "k-term finite continued
;;      fraction"--has the form

;;                  N_1
;;           -----------------
;;                     N_2
;;           D_1 + -----------
;;                 ...    N_K
;;                     + -----
;;                        D_K

;;      Suppose that `n' and `d' are procedures of one argument (the
;;      term index i) that return the n_i and D_i of the terms of the
;;      continued fraction.  Define a procedure `cont-frac' such that
;;      evaluating `(cont-frac n d k)' computes the value of the
;;      k-term finite continued fraction.  Check your procedure by
;;      approximating 1/[phi] using

;;           (cont-frac (lambda (i) 1.0)
;;                      (lambda (i) 1.0)
;;                      k)

;;      for successive values of `k'.  How large must you make `k' in
;;      order to get an approximation that is accurate to 4 decimal
;;      places?

;;   b. If your `cont-frac' procedure generates a recursive process,
;;      write one that generates an iterative process.  If it
;;      generates an iterative process, write one that generates a
;;      recursive process.

;;; Solution
;;; ========

;;; Implementing `cont-frac` function

(define (cont-frac n d k)
  (define (recur i)
    (if (= k i)
        0
        (/ (n i) (+ (d i) (recur (+ i 1))))))
  (recur 0))

(define (gr-aprox k)
  (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             k))

;;; Calculating how many steps required to get an aproximation that is accurate
;;; to 4 decimals

(define golden-ratio 1.6180)

(define (golden-ratio-aprox-steps)
  (define (close-enough? x y)
    (< (abs (- x y)) 0.0001))
  (define (try guess)
    (let ((next (gr-aprox guess)))
      (if (close-enough? next (/ 1 golden-ratio))
          guess
          (try (+ guess 1)))))
  (try 5))

;;; `golden-ratio-aprox-steps" function returns 9 steps required to get an aproximation that is
;;; accurate to 4 decimals

(golden-ratio-aprox-steps)              ;=> 9
(gr-aprox 9)
                                        ;=> 0.6179775280

;;; `cont-frac` iterative version

(define (cont-frac-iter n d k)
  (define (iter acc i)
    (if (= i 0)
        acc 
        (iter (/ (n i) (+ (d i) acc)) (- i 1))))
  (iter 0 k))

(define (gr-aprox-iter k)
  (cont-frac-iter (lambda (i) 1.0)
                  (lambda (i) 1.0)
                  k))

