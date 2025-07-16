#lang sicp

(define dx 1)
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3.0)))

(define (square x) (* x x))
((smooth square) 5)

(define (n-repeat f n)
  (if (= n 1)
      f
      (lambda (x) (f ((n-repeat f (- n 1)) x)))))

(define (n-fold-smooth f n)
  ((n-repeat smooth n) f))

((n-fold-smooth square 5) 5)
