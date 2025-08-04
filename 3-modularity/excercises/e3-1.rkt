#lang sicp

(define (make-accumulator sum)
  (lambda (add)
    (begin (set! sum (+ sum add))
           sum)))

(define A (make-accumulator 5))

(A 10)

(A 10)
