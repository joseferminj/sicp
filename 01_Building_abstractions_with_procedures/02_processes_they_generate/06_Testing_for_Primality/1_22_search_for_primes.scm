;; *Exercise 1.22:* Most Lisp implementations include a primitive
;; called `runtime' that returns an integer that specifies the amount
;; of time the system has been running (measured, for example, in
;; microseconds).  The following `timed-prime-test' procedure, when
;; called with an integer n, prints n and checks to see if n is
;; prime.  If n is prime, the procedure prints three asterisks
;; followed by the amount of time used in performing the test.

(define (square n)
  (* n n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))


(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (cond ((prime? n) (report-prime (- (current-inexact-milliseconds) start-time)))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;; Using this procedure, write a procedure `search-for-primes' that
;; checks the primality of consecutive odd integers in a specified
;; range.  Use your procedure to find the three smallest primes
;; larger than 1000; larger than 10,000; larger than 100,000; larger
;; than 1,000,000.  Note the time needed to test each prime.  Since
;; the testing algorithm has order of growth of [theta](_[sqrt]_(n)),
;; you should expect that testing for primes around 10,000 should
;; take about _[sqrt]_(10) times as long as testing for primes around
;; 1000.  Do your timing data bear this out?  How well do the data
;; for 100,000 and 1,000,000 support the _[sqrt]_(n) prediction?  Is
;; your result compatible with the notion that programs on your
;; machine run in time proportional to the number of steps required
;; for the computation?

;; Solution
;; ========
(define (search-for-primes min max)
  (cond ((divides? 2 min) (search-for-primes (+ min 1) max))
        ((< min max) (timed-prime-test min) (search-for-primes (+ min 2) max))))


(search-for-primes 1000 1020)           ; Output:
                                        ; 1009 *** 0.0029296875
                                        ; 1013 *** 0.003173828125
                                        ; 1019 *** 0.003173828125

(search-for-primes 10000 10020)         ; Output:
                                        ;10007 *** 0.008056640625
                                        ;10009 *** 0.008056640625
                                        ;10037 *** 0.008056640625

(search-for-primes 100000 100050)       ;Output:
                                        ; 100003 *** 0.026123046875
                                        ; 100019 *** 0.02587890625
                                        ; 100043 *** 0.02587890625
;; We can see how the calculation of prime number increments in _[sqrt]_(10) in
;; each range

