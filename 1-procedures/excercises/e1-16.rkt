#lang sicp

; iterative exponentiation
(define (exp x n)
  (define (even? n) (= (remainder n 2) 0))
  (define (exp-help result current n)
    (cond ((= n 0) result)
          ((even? n) (exp-help result (* current current) (/ n 2)))
          (else (exp-help (* result current) current (- n 1)))))
  (exp-help 1 x n))

(exp 100 0)
(exp 25 1)
(exp 3 3)
(exp 2 10)
(exp 2 4)
(exp 2 7)