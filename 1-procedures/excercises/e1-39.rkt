#lang sicp

(define (cont-frac n d k)
  (define (rec current)
    (if (> current k)
        0
        (/ (n current) (+ (d current) (rec (+ current 1))))))
  (rec 1))

(define (tan x k)
  (let ((x2 (* x x)))
    (cont-frac (lambda (i) (if (= i 1)
                               x
                               (- x2)))
               (lambda (i) (- (* 2 i) 1))
               k)))

(tan 0.8 10)
   
   
