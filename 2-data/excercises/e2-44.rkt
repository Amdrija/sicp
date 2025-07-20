#lang sicp

; Stubs
(define (below p1 p2) (p1)) 
(define (beside p1 p2) (p2))

(define (up-split painter n)
  (if (= n 0)
      painter
      ((let ((up (up-split painter (- n 1))))
        (below painter (beside up up))))))
