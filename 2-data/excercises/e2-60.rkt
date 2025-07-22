#lang sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set) (cons x set))

(define (intersection-set s1 s2)
  (cond ((or (null? s1) (null? s2)) '())
        ((element-of-set? (car s2) s1) 
         (cons (car s2) (intersection-set s1 (cdr s2))))
        (else (intersection-set s1 (cdr s2)))))

(define (union-set s1 s2)
  (if (null? s2)
      s1
      (adjoin-set (car s2) (union-set s1 (cdr s2)))))

(element-of-set? 4 (list 1 2 3 3 2))

(intersection-set (list 1 2 3 3 4) (list 3 3 4 5 6))
(intersection-set '() (list 2 3))
(intersection-set (list 4 5) '())
(intersection-set (list 1 2) (list 3 4))

(union-set (list 1 2 3 3 4) (list 3 4 5 6))
(union-set '() (list 2 3 3))
(union-set (list 4 5) '())
(union-set (list 1 2 1) (list 3 1 4))
