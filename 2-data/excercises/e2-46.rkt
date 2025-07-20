#lang sicp

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

(add-vect (make-vect 1 2) (make-vect 3 4))
(sub-vect (make-vect 1 2) (make-vect 3 4))
(scale-vect (make-vect 1 2) 2)
