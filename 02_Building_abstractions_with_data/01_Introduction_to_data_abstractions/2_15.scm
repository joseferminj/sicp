;; Exercise 2.15.  Eva Lu Ator, another user, has also noticed the
;; different intervals computed by different but algebraically
;; equivalent expressions. She says that a formula to compute with
;; intervals using Alyssa's system will produce tighter error bounds
;; if it can be written in such a form that no variable that
;; represents an uncertain number is repeated. Thus, she says, par2 is
;; a ``better'' program for parallel resistances than par1. Is she
;; right? Why?


;;; Solution
;;; ========

;;; Acording to Wikipedia
;;; http://en.wikipedia.org/wiki/Interval_arithmetic#Dependency_problem
;;;
;;; So par2 is ``better`` because each variable appears only once. The
;;; par1 leads to an unwanted expansion of the resulting intervals
;;; because an interval occurs several times in the calculation.

