#lang sicp

(define (filtered-accumulate combiner null-value a b term next filter?)
  (cond ((> a b) null-value)
        ((filter? a) (combiner (term a)
                               (filtered-accumulate combiner null-value (next a) b term next filter?)))
        (else (filtered-accumulate combiner null-value (next a) b term next filter?))))

(define (prime? n)
  (define (prime-iter n factor)
    (cond ((> (* factor factor) n) true)
          ((= (remainder n factor) 0) false)
          (else (prime-iter n (+ factor 1)))))
  (prime-iter n 2))

(define (square x) (* x x))
(define (inc x) (+ x 1))
(define (sum-square-prime a b)
  (filtered-accumulate + 0 a b square inc prime?))

(sum-square-prime 2 10)

(define (gcd a b)
  (if (= b 0) 
      a
      (gcd b (remainder a b))))
; the product of all the positive integers 
; less than n that are relatively prime to n 
; (i.e., all positive integers i < n such that GCD ( i , n ) = 1 ). 
(define (pppil n)
  (define (rel-prime? x) (= (gcd x n) 1))
  (define (identity x) x)
  (filtered-accumulate * 1 1 (- n 1) identity inc rel-prime?))

(pppil 10)
  
