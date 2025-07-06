#lang sicp

(define (abs x) 
  (if (< x 0) (- x) x))
(define (rel-delta guess x) (abs (/ (- guess x) x)))
(define (good-enough? guess x) (< (rel-delta guess x) 0.00001))

(define (square x) (* x x))
(define (square-deriv guess x) (/ x guess))

(define (cube x) (* x x x))
(define (cube-deriv guess x) 
  (/ (+ (/ x (square guess)) 
        (* 2 guess)) 
     3))

(define (average x y) (/ (+ x y) 2))
(define (improve guess x derivative) (average (derivative guess x) guess))


(define (newton-iter guess x func derivative) 
  (if (good-enough? (func guess) x)
      guess
      (newton-iter (improve guess x derivative) x func derivative)))
(define (sqrt x) (newton-iter 1.0 x square square-deriv))
(define (cbrt x) (newton-iter 1.0 x cube cube-deriv))

(sqrt 2)
(sqrt 4)
(sqrt 0.0001)

(cbrt 27)
(cbrt 64)
(cbrt 3)