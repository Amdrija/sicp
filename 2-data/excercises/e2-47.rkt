#lang sicp

(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2))

(define origin-frame1 car)
(define edge1-frame1 cadr)
(define edge2-frame1 caddr)

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define origin-frame2 car)
(define edge1-frame2 cadr)
(define edge2-frame2 cddr)

(define v (make-frame1 1 2 3))
(origin-frame1 v)
(edge1-frame1 v)
(edge2-frame1 v)

(define w (make-frame2 4 5 6))
(origin-frame2 w)
(edge1-frame2 w)
(edge2-frame2 w)
