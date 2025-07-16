#lang sicp

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-rec p1 p2) (cons p1 p2))
(define (p1-rec rec) (car rec))
(define (p2-rec rec) (cdr rec))

(define (height rec)
  (abs (- (y-point (p1-rec rec)) 
          (y-point (p2-rec rec)))))
(define (width rec)
  (abs (- (x-point (p1-rec rec))
          (x-point (p2-rec rec)))))

(define (perimeter rec)
  (* 2 (+ (height rec) (width rec))))

(define (area rec)
  (* (height rec) (width rec)))

(define r (make-rec (make-point 4 5) (make-point 1 1)))
(perimeter r)
(area r)