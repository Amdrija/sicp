#lang sicp

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (fringe tree)
  (cond ((null? tree) (list))
        ((pair? (car tree)) (append (fringe (car tree)) (fringe (cdr tree))))
        (else (cons (car tree) (fringe (cdr tree))))))

(define x 
  (list (list 1 2) (list 3 4)))

(fringe x)