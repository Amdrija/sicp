#lang sicp

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree 
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size 
             (quotient (- n 1) 2)))
        (let ((left-result 
               (partial-tree 
                elts left-size)))
          (let ((left-tree 
                 (car left-result))
                (non-left-elts 
                 (cdr left-result))
                (right-size 
                 (- n (+ left-size 1))))
            (let ((this-entry 
                   (car non-left-elts))
                  (right-result 
                   (partial-tree 
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree 
                     (car right-result))
                    (remaining-elts 
                     (cdr right-result)))
                (cons (make-tree this-entry 
                                 left-tree 
                                 right-tree)
                      remaining-elts))))))))

; The (partial-tree elts n) procedure first find the
; n / 2 element in elts and uses it as the root of the tree
; Then, it calls (partial-tree elts (n / 2 - 1)) to construct
; a balanced tree of the first n/2 elemnts. This would be the
; left subtree. The result also contains the unused elements
; in the left subtree. It then calls (partial-tree unused-elts (n - left-size - 1))
; to construct the right subtree.
;
; Output
; (partial-tree (1 3 5 7 9 11) 6)
; => left-size = 5 / 2 = 2
; => left-result = (partia-tree (1 3 5 7 9 11) 2)
;       => left-size = 1 / 2 = 0
;       => left-resullt = (partial-tree (1 3 5 7 9 11) 0) = (() (1 3 5 7 9 11))
;       => left-tree = ()
;       => non-left-elts = (1 3 5 7 9 11)
;       => right-size = 2 - 1 = 1
;       => this-entry = 1
;       => right-result = (partial-tree (3 5 7 9 11) 1)
;               => left-size = 0 / 2 = 0
;               => left-result = (partial-tree (3 5 7 9 11) 0) = (() (3 5 7 9 11))
;               => left-tree = ()
;               => non-left-elts = (3 5 7 9 11)
;               => right-size = 1 - 1 = 0
;               => this-entry = 3
;               => right-result = (partial-tree (5 7 9 11) 0) = (() (5 7 9 11))
;               => right-tree = ()
;               => remaining-elts = (5 7 9 11)
;               => return ((3 () ()) (5 7 9 11))
;       => right-result = ((3 () ()) (5 7 9 11))
;       => right-tree = (3 () ())
;       => remaining-elts = (5 7 9 11)
;       => return ((1 () (3 () ())) (5 7 9 11))
; => left-result = ((1 () (3 () ())) (5 7 9 11))
; => left-tree = (1 () (3 () ()))
; => non-left-elts = (5 7 9 11)
; => right-size = 6 - 3 = 3
; => this-entry = 5
; => right-result = (partial-tree (7 9 11) 3)
;       => left=size = 2 / 2 = 1
;       => left-result = (partial-tree (7 9 11) 1) = ... = ((7 () ()) (9 11))
;       => left-tree = (7 () ())
;       => non-left-elts = (9 11)
;       => right-size = 3 - 2 = 1
;       => this-entry = 9
;       => right-result = (partial-tree (11) 1) = ... = ((11 () ()) ())
;       => right-tree = (11 () ())
;       => remaining-elts = ()
;       => return ((9 (7 () ()) (11 () ())) ())
; => right-tree = (9 (7 () ()) (11 () ()))
; => remaining-elts = ()
; => return ( (5 (1 () (3 () ())) (9 (7 () ()) (11 () ()))) () )
; Produced tree
;       5
;      / \
;     /   \
;    1     9
;     \   / \
;      3 7  11
