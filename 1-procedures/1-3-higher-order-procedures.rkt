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


; Half interval method for searching roots of functions
; i.e x such that f(x) = 0
(define (average x y) (/ (+ x y) 2))
(define (close-enough? x y) 
  (< (abs (- x y)) 0.001))
(define (search f neg-point pos-point)
  (let ((midpoint 
         (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond 
           ((positive? test-value)
            (search f neg-point midpoint))
           ((negative? test-value)
            (search f midpoint pos-point))
           (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) 
                (positive? b-value))
           (search f a b))
          ((and (negative? b-value) 
                (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of 
                   opposite sign" a b)))))

(half-interval-method sin 2.0 4.0)
(half-interval-method 
 (lambda (x) (- (* x x x) (* 2 x) 3))
 1.0
 2.0)

