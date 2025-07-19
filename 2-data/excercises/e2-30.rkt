#lang sicp

(define (square x) (* x x))

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((pair? tree) (cons (square-tree (car tree))
                            (square-tree (cdr tree))))
        (else (square tree))))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (square-tree-map tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (square-tree-map subtree)
             (square subtree))) tree))

(square-tree-map
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
