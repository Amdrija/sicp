#lang sicp

(define (n-repeat f n)
  (if (= n 1)
      f
      (lambda (x) (f ((n-repeat f (- n 1)) x)))))

((n-repeat inc 10) 5)
((n-repeat (lambda (x) (* x x)) 2) 5)
