#lang sicp

(define (adjoin-set x set)
  (cond ((null? set) (cons x nil))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(adjoin-set 1 '())
(adjoin-set 2 '(1 3))
(adjoin-set 4 '(1 2 3))
(adjoin-set 3 '(1 2 3 4))
(adjoin-set 2 '(1 2 3 4))
(adjoin-set 0 '(1 2 3 4))
