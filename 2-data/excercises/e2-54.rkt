#lang sicp

(define (equal? l1 l2)
  (cond ((and (null? l1) (null? l2)) true)
        ((null? l1) false)
        ((null? l2) false)
        ((not (eq? (car l1) (car l2))) false)
        (else (equal? (cdr l1) (cdr l2)))))

(equal? (list) (list))
(equal? '() '(1 2 3))
(equal? '(1) '())
(equal? (list 1 2 4) (list 1 2 3))
(equal? (list 1 2 3 4) (list 1 2 3))
(equal? (list 1 2 3) (list 1 2 3 4))
(equal? (list 1 2 3 4) (list 1 2 3 4))
(equal? (list 'ab 'cd) (list 'ab 'cd))
