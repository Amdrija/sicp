#lang sicp

(define (reverse items)
  (define (reverse-iter reversed items)
    (if (null? items)
        reversed
        (reverse-iter (cons (car items) reversed) (cdr items))))
  (reverse-iter nil items))

(reverse (list 1 2 3 4))