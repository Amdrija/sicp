#lang sicp

(define (install-raises)
  (define (integer->rational n)
    (make-rational n 1))

  (define (rational->real n)
    (/ (numer n) (denom n)))

  (define (real->complex n)
    (make-from-real-imag n 0))
  (put 'raise 'integer integer->rational)
  (put 'raise 'rational rational->real)
  (put 'raise 'real real->complex))

(define (raise x) (apply-generic 'raise x))
