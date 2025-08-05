#lang sicp

(define (has-cycle? list)
  (define (iter fast slow)
    (cond ((null? fast) false)
          ((null? (cdr fast)) false)
          ((eq? fast slow) true)
          (else (iter (cddr fast) (cdr slow)))))
  (iter (cdr list) list))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define cycle1 (list 1))
(set-cdr! cycle1 cycle1)
(has-cycle? cycle1)

(define cycle2 (list 1 2 3 4 5 6))
(set-cdr! (last-pair cycle2) cycle2)
(has-cycle? cycle2)

(has-cycle? (list 1 2 3 4 5))
(has-cycle? (list 1))

