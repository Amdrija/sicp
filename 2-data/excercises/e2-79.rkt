#lang sicp

(define (equ n1 n2)
  (apply-generic 'equ n1 n2))

; Then each package needs an equality and install it

; Scheme numbers
(define (install-scheme-pkg)
  (put 'equ ('scheme-number 'scheme-number) (lambda (n1 n2) (= n1 n1))))

; Complex numbers
(define (install-complex-pkg)
  (define (eq c1 c2)
    (and (= (real-part c1) (real-part c2)) (= (imag-part c1) (imag-part c2))))
  (put 'equ ('complex 'complex) eq))
