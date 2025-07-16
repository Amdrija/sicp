#lang sicp

(define (make-rat n d)
  (let ((g (gcd n d)) (nom (abs n)) (denom (abs d)))
    (if (< (* n d) 0)
        (cons (/ (- nom) g) (/ denom g))
        (cons (/ nom g) (/ denom g)))))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(print-rat (make-rat -4 -2))
(print-rat (make-rat 4 -2))
(print-rat (make-rat -4 2))
(print-rat (make-rat 4 2))
