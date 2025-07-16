#lang sicp

(define (fixed-point f first-guess)
  (define (close-enough? x y) (< (abs (- x y)) 0.0001))
  (define (try-guess guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try-guess next))))
  (try-guess first-guess))

(define (gold-ratio x) (+ 1 (/ 1 x)))
(define (xx x) (/ (log 1000) (log x)))
(fixed-point gold-ratio 1.0)
(newline)
(fixed-point xx 2.0)

(define (average x y) (/ (+ x y) 2))
(define (avg-damp f) (lambda (x) (average x (f x))))
(newline)
(fixed-point (avg-damp gold-ratio) 1.0)
(newline)
(fixed-point (avg-damp xx) 2.0)
