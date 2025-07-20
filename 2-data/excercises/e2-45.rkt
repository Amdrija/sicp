#lang sicp

; Stubs
(define (below p1 p2) (p1)) 
(define (beside p1 p2) (p2))

(define (split p1 p2)
  (define (split-rec painter n)
    (if (= n 0)
        painter
        (let ((smaller (split-rec painter (- n 1))))
          (p1 painter (p2 smaller smaller)))))
  split-rec)

(define right-split (split beside below))
(define up-split (split below beside))
