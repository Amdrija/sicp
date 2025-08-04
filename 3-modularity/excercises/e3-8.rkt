#lang sicp

(define state 1)
(define (f x) (begin (set! state (* state x)) state))

(+ (f 0) (f 1))
(set! state 1)
(+ (f 1) (f 0)) ; equivalent to evaluation from right to left
