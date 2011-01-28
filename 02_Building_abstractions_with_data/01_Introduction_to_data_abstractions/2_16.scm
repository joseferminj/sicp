;;; Exercise 2.16.  Explain, in general, why equivalent algebraic
;;; expressions may lead to different answers. Can you devise an
;;; interval-arithmetic package that does not have this shortcoming,
;;; or is this task impossible? (Warning: This problem is very
;;; difficult.)

;;; Solution
;;; ========
;;;
;;; According to the Wikipedia
;;; http://en.wikipedia.org/wiki/Interval_arithmetic#Dependency_problem
;;;
;;; In general, it can be shown that the exact range of values can be
;;; achieved, if each variable appears only once. However, not every
;;; function can be rewritten this way.
