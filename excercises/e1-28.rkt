#lang sicp

(define (even? x) (= (remainder x 2) 0))
(define (square x) (* x x))

(define (expmod base n)
  (define (exp-iter result exp_curr)
    (cond ((= exp_curr 0) 1)
          ((and (not (= result 1))
                (not (= result (- n 1)))
                (= (remainder (square result) n) 1)) 2)
          ((even? exp_curr) (exp-iter (remainder (square result) n) (/ exp_curr 2)))
          (else (exp-iter (* result base) (- exp_curr 1)))))
  (exp-iter 1 (- n 1)))

(define (fermat-test? n)
  (define (iter current)
    (cond ((= current n) true)
          ((not (= (expmod current n) 1)) false)
          (else (iter (+ current 1)))))
  (iter 2))

(fermat-test? 561)
(fermat-test? 1105)
(fermat-test? 37)
(fermat-test? 10007)