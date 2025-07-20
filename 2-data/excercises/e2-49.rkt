#lang sicp

; Stub
(define (segment-painter segment-list) (lambda(frame) frame))

; Vectors
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect v w) 
  (make-vect (+ (xcor-vect v) (xcor-vect w))
             (+ (ycor-vect v) (ycor-vect w))))

(define (scale-vect v c)
  (make-vect (* c (xcor-vect v)) (* c (ycor-vect v))))

(define (neg-vect v)
  (scale-vect v -1))

(define (sub-vect v w)
  (add-vect v (neg-vect w)))

(define (mid-point-vect v w)
  (scale-vect (add-vect v w) 0.5))

; Frames
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)

(define (corner-edge1-frame frame) (add-vect (origin-frame frame) (edge1-frame frame)))
(define (corner-edge2-frame frame) (add-vect (origin-frame frame) (edge2-frame frame)))
(define (corner-edge1edge2-frame frame) (add-vect (corner-edge1-frame frame) (edge2-frame frame)))

; Segments
(define (make-segment start end) (cons start end))
(define start-segment car)
(define end-segment cdr)

(define outline-painter
  (segment-painter (list (make-segment (make-vect 0 0) (make-vect 0 1))
                         (make-segment (make-vect 0 0) (make-vect 1 0))
                         (make-segment (make-vect 0 1) (make-vect 1 1))
                         (make-segment (make-vect 1 0) (make-vect 1 1))))) 

(define x-painter
  (segment-painter (list (make-segment (make-vect 0 0) (make-vect 1 1))
                         (make-segment (make-vect 0 1) (make-vect 1 0)))))

(define diamond-painter
  (segment-painter (list (make-segment (make-vect 0.5 0) (make-vect 0 0.5))
                         (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
                         (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
                         (make-segment (make-vect 1 0.5) (make-vect 0.5 0)))))

; Wave painter too tedious