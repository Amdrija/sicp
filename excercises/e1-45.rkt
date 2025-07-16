#lang sicp

(define (n-repeat f n)
  (if (= n 1)
      f
      (lambda (x) (f ((n-repeat f (- n 1)) x)))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.0001))
(define (fixed-point f guess)
  (let ((next (f guess)))
    (if (close-enough? next guess)
        next
        (fixed-point f next))))

(define (average x y) (/ (+ x y) 2))
(define (avg-damp f) (lambda (x) (average x (f x))))

(define (n-root x n)
  (define (equation y) (/ x (expt y (- n 1))))
  (fixed-point ((n-repeat avg-damp n) equation) 1.0))

(n-root 256 2)
