#lang sicp

(define (sum-range a b func next)
  (define (sum-range-iter sum current)
    (if (> current b)
        sum
        (sum-range-iter (+ sum (func current))
                        (next current))))
  (sum-range-iter 0 a))