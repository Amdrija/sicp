#lang sicp

(define (sum-of-sqaures x y) (+ (* x x) (* y y)))
(define (sum-of-squares-largest-2 x y z) 
  (cond ((and (<= x y) (<= x z)) (sum-of-sqaures y z)) 
        ((and (<= y x) (<= y z)) (sum-of-sqaures x z))
        (else (sum-of-sqaures x y))))

(= (sum-of-squares-largest-2 1 2 3) 13)
(= (sum-of-squares-largest-2 1 6 3) 45)
(= (sum-of-squares-largest-2 4 1 5) 41)
(= (sum-of-squares-largest-2 4 1 3) 25)
(= (sum-of-squares-largest-2 6 7 1) 85)
(= (sum-of-squares-largest-2 5 2 1) 29)

(= (sum-of-squares-largest-2 3 3 2) 18)
(= (sum-of-squares-largest-2 4 1 4) 32)
(= (sum-of-squares-largest-2 5 5 4) 50)

(= (sum-of-squares-largest-2 6 6 6) 72)
