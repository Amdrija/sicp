#lang sicp

(define (product a b f next)
  (if (> a b)
      1
      (* (f a)
         (product (next a) b f next))))

(define (factorial n)
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (product 1 n identity inc))

(factorial 6)
