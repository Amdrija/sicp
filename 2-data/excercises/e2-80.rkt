#lang sicp

(define (install-zero-pkg)
  (put '=zero? 'scheme-number) (lambda(x)) (= x 0)
  (put '=zero? 'rational-number) (lambda (r) (= (numer r) 0)))
