#lang sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (union-set s1 s2)
  (if (null? s2)
      s1
      (adjoin-set (car s2) (union-set s1 (cdr s2)))))

(union-set (list 1 2 3 4) (list 3 4 5 6))
(union-set '() (list 2 3))
(union-set (list 4 5) '())
(union-set (list 1 2) (list 3 4))
