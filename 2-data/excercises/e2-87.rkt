#lang sicp

(define (install-zero)
  (define (poly-zero? p)
    (define (all-zero? termlist)
      (or (empty-termlist? termlist)
          (and (=zero? (coeff (first-term term-list)))
               (all-zero (rest-terms term-list)))))
    (all-zero? (term-list p)))
  (put '=zero? '(polynomial) poly-zero?)) 
