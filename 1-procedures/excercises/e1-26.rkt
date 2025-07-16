#lang sicp

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder 
          (* (expmod base (/ exp 2) m)
             (expmod base (/ exp 2) m))
          m))
        (else
         (remainder 
          (* base 
             (expmod base (- exp 1) m))
          m))))

; Because we are calling expmod twice here 
; in order to square it, the interpreter will
; first evaluate it twice and only then calculate
; the product. This means our optimisation doesn't
; work at all and the program becomes linear time.