#lang sicp

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
(define (average-point p q)
  (define (average a b) (/ (+ a b) 2.0))
  (make-point (average (x-point p) (x-point q))
              (average (y-point p) (y-point q))))

(define (make-segment start end) (cons start end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (mid-point-segment s)
  (average-point (start-segment s) (end-segment s)))

(define s (make-segment (make-point 1 4) (make-point 2 2)))
(print-point (mid-point-segment s))
