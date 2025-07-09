#lang sicp

(define (accumulate combiner null-value a b term next)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value (next a) b term next))))

(define (accumulate-iter combiner null-value a b term next)
  (define (iter result current)
    (if (> current b)
        result
        (iter (combiner (term current) result) (next current))))
  (iter null-value a))

(define (sum a b term next)
  (accumulate + 0 a b term next))

(define (product a b term next)
  (accumulate-iter * 1 a b term next))

(define (identity x) x)
(define (inc x) (+ x 1))

(sum 1 10 identity inc)
(product 1 6 identity inc)

