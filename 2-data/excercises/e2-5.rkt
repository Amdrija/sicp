#lang sicp

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (number-fract p n)
  (define (iter curr p)
    (if (= (remainder p n) 0)
        (iter (+ curr 1) (/ p n))
        curr))
  (iter 0 p))

(define (car p) (number-fract p 2))
(define (cdr p) (number-fract p 3))

(car (cons 1 2))
(cdr (cons 4 5))
