#lang sicp

(define (mul a b)
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))
  (define (even? x) (= (remainder x 2) 0))
  (cond ((= a 0) 0)
        ((even? a) (double(mul (halve a) b)))
        (else (+ b (mul (- a 1) b)))))

(mul 2 4)
(mul 7 9)
(mul 6 5)
(mul 3 8)
(mul 0 5)
(mul 7 0)
(mul 1 12)
(mul 14 1)