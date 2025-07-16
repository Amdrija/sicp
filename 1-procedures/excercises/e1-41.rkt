#lang sicp

(define (double f)
  (lambda (x) (f (f x))))

(((double double) inc) 5)
(((double (double double)) inc) 5)
((double (double (double inc))) 5)

;(double double) -> (lambda (x) (double (double x))
; (double (lambda (x) (double (dobule x))))
; -> (lambda (x) (lambda(x) (double double (x))
