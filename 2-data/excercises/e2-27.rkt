#lang sicp

(define (reverse tree)
  (define (iter result tree)
    (cond ((null? tree) result)
          ((not (pair? tree)) tree)
          (else (iter (cons (reverse (car tree)) result) (cdr tree)))))
  (iter nil tree))

(reverse (list 1 2 3 4))
(reverse (list (list 1 2) (list 3 4)))
