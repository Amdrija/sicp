#lang sicp

(define (smallest-divisor n)
  (define (divides? n factor) (= (remainder n factor) 0))
  (define (smallest-d-iter n factor)
    (cond ((> (* factor factor) n) n)
          ((divides? n factor) factor)
          (else (smallest-d-iter n (+ factor 1)))))
  (smallest-d-iter n 2))
        
(smallest-divisor 289)
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)