#lang sicp

(define (fixed-point f first-guess)
  (define (close-enough? x y) (< (abs (- x y)) 0.0001))
  (define (try-guess guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try-guess next))))
  (try-guess first-guess))

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)