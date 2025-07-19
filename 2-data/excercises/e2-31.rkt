#lang sicp

(define (tree-map f tree)
  (cond ((null? tree) nil)
        ((pair? tree) (cons (tree-map f (car tree))
                           (tree-map f (cdr tree))))
        (else (f tree))))

(define (square x) (* x x))

(define tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(tree-map square tree)

(define (tree-map2 f tree)
  (map (lambda(subtree) (if (pair? subtree)
                            (tree-map2 f subtree)
                            (f subtree))) tree))

(tree-map2 square tree)
