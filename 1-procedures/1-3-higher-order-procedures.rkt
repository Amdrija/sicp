#lang sicp

; sum of range

(define (sum-range a b func next)
  (if (> a b)
      0
      (+ (func a)
         (sum-range (next a) b func next))))

(define (square x) (* x x))
(define (cube x) (* x x x))
(define (inc x) (+ x 1))

(sum-range 1 10 square inc)
(sum-range 1 10 cube inc)

(define (integral f a b dx)
  (define (next x) (+ x dx))
  (define (val x) (* (f (+ x (/ dx 2))) dx))
  (sum-range a b val next))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)
(integral square 1 2 0.001)
