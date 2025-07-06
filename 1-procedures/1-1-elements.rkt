#lang sicp

; complex expression
(+ (* 3
      (+ (* 2 4)
         (+ 3 5)))
   (+ (- 10 7)
      6))

; variables
(define size 2)
(* 5 size)

; procedures
(define (square x) (* x x))
(square 5)

(define (sum-of-sqaures x y)
  (+ (square x) (square y)))
(sum-of-sqaures 3 4)

; conditional statements
(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (abs2 x)
  (cond ((< x 0) (- x))
        (else x)))

(define (max a b)
  (cond (a < b) (b)
    (else a)))

(define (abs3 x)
  (if (< x 0)
      (- x)
      x))

(define x 2)
(and (> x 5) (< x 10))
 
(define (>= x y) 
  (or (> x y) (= x y)))
(abs -5)
(max 10 5)
(max 5 10)

; sqrt
(define (average x y) (/ (+ x y) 2))
(define (sqrt x)
  (define (good-enough? guess) 
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess) 
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess 
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(sqrt 4)
(sqrt 0.00001)
