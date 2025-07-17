#lang sicp

(define (same-parity . nums)
  (define first (car nums))
  (define (same-parity-first x) (= (remainder x 2) (remainder first 2)))
  (define (rec nums)
    (cond ((null? nums) nil)
          ((same-parity-first (car nums)) (cons (car nums) (rec (cdr nums))))
          (else (rec (cdr nums)))))
  (rec nums))

(same-parity 1 2 3 4 5 6 7)

(same-parity 2 3 4 5 6 7)
