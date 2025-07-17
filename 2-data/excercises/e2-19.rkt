#lang sicp

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((< amount 0) 0)
        ((null? coin-values) 0)
        (else (+ (cc amount (cdr coin-values))
                 (cc (- amount (car coin-values)) coin-values)))))

(define us-coins 
  (list 50 25 10 5 1))

(define uk-coins 
  (list 100 50 20 10 5 2 1 0.5))

(cc 100 us-coins)
(cc 100 uk-coins)
(cc 20 uk-coins)

(define us-coins-unordered (list 5 10 25 1 50))
(cc 100 us-coins-unordered)