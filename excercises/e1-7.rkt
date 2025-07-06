#lang sicp
(define (abs x) 
  (if (< x 0) (- x) x))
(define (rel-delta guess x) (abs (/ (- guess x) x)))
(define (good-enough? guess x) (< (rel-delta guess x) 0.00001))

(define (average x y) (/ (+ x y) 2))
(define (improve guess x) (average (/ x guess) guess))

(define (square x) (* x x))
(define (square-iter guess x) 
  (if (good-enough? (square guess) x)
      guess
      (square-iter (improve guess x) x)))
(define (sqrt x) (square-iter 1.0 x))

(sqrt 2)
(sqrt 4)
(sqrt 0.0001)
