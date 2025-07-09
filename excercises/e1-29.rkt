#lang sicp

(define (sum-range a b func next)
  (define (sum-range-iter sum current)
    (if (> current b)
        sum
        (sum-range-iter (+ sum (func current))
                        (next current))))
  (sum-range-iter 0 a))

(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (next x) (+ x h h))
  (* (+ (f a)
        (* 2 (sum-range (+ a h h) (- b h) f next))
        (* 4 (sum-range (+ a h) b f next))
        (f b)) 
     (/ h 3)))

(define (square x) (* x x))
(define (cube x) (* x x x))
(integral cube 0.0 1.0 6)
(integral cube 0.0 1.0 100)
(integral square 1.0 2.0 100)

