#lang sicp

(define (close-enough? x y)
  (< (abs (- x y)) 0.0001))

(define (fixed-point f guess)
  (let ((next (f guess)))
    (if (close-enough? next guess)
        next
        (fixed-point f next))))

(define dx 0.000001)
(define (deriv f)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) 
                 dx)))
(define (newton-transform f)
  (lambda (x) (- x (/ (f x) 
                      ((deriv f) x)))))

(define (newton-method f guess)
  (fixed-point (newton-transform f) guess))

(define (sqrt x)
  (newton-method (lambda (y) (- (* y y) x)) 1.0))

(sqrt 25)

(define (cubic a b c)
  (lambda (x) (+ (* x x x)
                 (* a x x)
                 (* b x)
                 c)))

(newton-method (cubic 1.0 1.0 1.0) 1)
