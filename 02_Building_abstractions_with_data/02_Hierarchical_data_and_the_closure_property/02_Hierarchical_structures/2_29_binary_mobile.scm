;; Exercise 2.29.  A binary mobile consists of two branches, a left
;; branch and a right branch. Each branch is a rod of a certain
;; length, from which hangs either a weight or another binary
;; mobile. We can represent a binary mobile using compound data by
;; constructing it from two branches (for example,using list):

(define (make-mobile left right)
  (list left right))

;; A branch is constructed from a length (which must be a number)
;; together with a structure, which may be either a number
;; (representing a simple weight) or another mobile:

(define (make-branch length structure)
  (list length structure))

;; a.  Write the corresponding selectors left-branch and right-branch,
;; which return the branches of a mobile, and branch-length and
;; branch-structure, which return the components of a branch.

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-structure branch)
  (cadr branch))

(define (branch-length branch)
  (car branch))

(define (mobile? structure)
  (pair? structure))

;; b.  Using your selectors, define a procedure total-weight that
;; returns the total weight of a mobile.

(define (total-weight mobile)
  (define (weight-branch b)
    (let ([structure (branch-structure b)]
          [length (branch-length b)])
      (cond [(not (mobile? structure)) structure]
            [else (total-weight structure)])))
  (+ (weight-branch (left-branch mobile))
     (weight-branch (right-branch mobile))))

;;; Unit Testing
;;; ============
;;; Tests from http://community.schemewiki.org/?sicp-ex-2.29

;; A test mobile: 
;; Level 
;; ----- 
;; 3                   4  |    8                                      
;;              +---------+--------+ 2                        
;; 2         3  |  9                                        
;;        +-----+----+ 1                                    
;; 1    1 | 2                                       
;;    +---+---+                             
;;    2       1                             


(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(define level-1-mobile (make-mobile (make-branch 2 1) 
                                    (make-branch 1 2))) 
(define level-2-mobile (make-mobile (make-branch 3 level-1-mobile) 
                                    (make-branch 9 1))) 
(define level-3-mobile (make-mobile (make-branch 4 level-2-mobile) 
                                    (make-branch 8 2))) 

(define mobile-weight-tests
  (test-suite
   "Mobile binary total weigth  tests. 2.29 sicp exercise"
   (check-equal?(total-weight level-1-mobile) 3)
   (check-equal?(total-weight level-2-mobile) 4)
   (check-equal?(total-weight level-3-mobile) 6)))

  
;; c.  A mobile is said to be balanced if the torque applied by its
;; top-left branch is equal to that applied by its top-right branch
;; (that is, if the length of the left rod multiplied by the weight
;; hanging from that rod is equal to the corresponding product for the
;; right side) and if each of the submobiles hanging off its branches
;; is balanced. Design a predicate that tests whether a binary mobile
;; is balanced.

(define (balanced? mobile)
  (define (branch-balanced? b)
    (cond [(mobile? (branch-structure b))(balanced? (branch-structure b))]
          [else true]))
  (define (weight-branch b)
    (let ([structure (branch-structure b)]
          [length (branch-length b)])
      (cond [(not (mobile? structure)) structure]
            [else (total-weight structure)])))
  (define (mul b)
    (* (branch-length b) (weight-branch b)))
  
  (let ([l (left-branch mobile)]
        [r (right-branch mobile)])
    (and (= (mul l) (mul r))
         (branch-balanced? l)
         (branch-balanced? r))))
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))


;; d.  Suppose we change the representation of mobiles so that the
;; constructorsare


(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

;; How much do you need to change your programs to convert to the new
;; representation?

;;; Solution
;;; ========

;;; The new representation forces changing the accesors

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

