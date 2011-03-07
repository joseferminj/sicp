;;; Exercise 2.65.  Use the results of exercises 2.63 and  2.64 to
;;; give (n) implementations of union-set and intersection-set for
;;; sets implemented as (balanced) binarytrees.41

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (union-set s1 s2)
  (cond [(null? s1) s2]
        [(null? s2) s1]
        [(< (car s1) (car s2)) (cons (car s1) (union-set (cdr s1) s2))]
        [else (cons (car s2) (union-set s1 (cdr s2)))]))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(define (union-set-tree s1 s2)
  (let ([l1 (tree->list s1)]
        [l2 (tree->list s2)])
    (list->tree (union-set l1 l2))))

(define (intersection-set-tree s1 s2)
  (let ([l1 (tree->list s1)]
        [l2 (tree->list s2)])
    (list->tree (intersection-set l1 l2))))


