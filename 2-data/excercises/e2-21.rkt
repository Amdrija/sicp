#lang sicp

(define (square x) (* x x))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (square-list-imper items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list-imper (cdr items)))))

    
(define (square-list-map items)
  (map square items))

(square-list-imper (list 1 2 3 4))
(square-list-map (list 1 2 3 4))
