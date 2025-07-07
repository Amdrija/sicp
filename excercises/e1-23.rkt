#lang sicp

(define (smallest-divisor-fast n)
  (define (next factor)
    (if (= factor 2)
        3
        (+ factor 2)))
  (define (divides? n factor) (= (remainder n factor) 0))
  (define (smallest-d-iter n factor)
    (cond ((> (* factor factor) n) n)
          ((divides? n factor) factor)
          (else (smallest-d-iter n (next factor)))))
  (smallest-d-iter n 2))

(define (smallest-divisor n)
  (define (divides? n factor) (= (remainder n factor) 0))
  (define (smallest-d-iter n factor)
    (cond ((> (* factor factor) n) n)
          ((divides? n factor) factor)
          (else (smallest-d-iter n (+ factor 1)))))
  (smallest-d-iter n 2))

(define (time1 f arg)
  (define start (runtime))
  (f arg)
  (display "Time: ")
  (display (- (runtime) start))
  (display "us")
  (newline))

(time1 smallest-divisor-fast 10000000033)
(time1 smallest-divisor 10000000033)