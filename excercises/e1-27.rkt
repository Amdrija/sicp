#lang sicp

(define (even? x) (= (remainder x 2) 0))
(define (square x) (* x x))

(define (expmod base exp mod)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder
                       (square (expmod base (/ exp 2) mod))
                       mod))
        (else (remainder
                (* (expmod base (- exp 1) mod) base)
                mod))))

(define (fermat-test? n)
  (define (iter current)
    (cond ((= current n) true)
          ((not (= (expmod current n n) current)) false)
          (else (iter (+ current 1)))))
  (iter 2))

(define (prime? n)
  (define (prime-iter n factor)
    (cond ((> (* factor factor) n) true)
          ((= (remainder n factor) 0) false)
          (else (prime-iter n (+ factor 1)))))
  (prime-iter n 2))

(define (carmichael? n) (and (fermat-test? n) (not (prime? n))))

(carmichael? 561)
(carmichael? 1105)
(carmichael? 37)