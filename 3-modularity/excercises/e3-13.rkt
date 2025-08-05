#lang sicp

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

; 'a : |
;  |    b : |
;  |        c : |
;  '------------'

(last-pair z) ; Infinite loop, because of tail-call optimisation (but in other language stack overflow)