#lang sicp

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

; mystery reverses the list in place and sets 
; the passed list to only contain the first element
(define v (list 'a 'b 'c 'd))
(define w (mystery v))
w
v