#lang sicp

(define (iterative-improve good-enough? improve)
  (define (rec guess)
    (if (good-enough? guess)
        guess
        (rec (improve guess))))
  rec)

(define (fixed-point f x)
  (define (close-enough? guess) (< (abs (- (f guess) guess)) 0.001))
  ((iterative-improve close-enough? f) x))

(fixed-point cos 1.0)

(define (sqrt x)
  (define (average a b) (/ (+ a b ) 2))
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

(sqrt 9)
