#lang sicp

(define one-through-four (list 1 2 3 4))

(car one-through-four)

(cdr one-through-four)

(car (cdr one-through-four))

(cons 10 one-through-four)

(cons 5 one-through-four)

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(list-ref one-through-four 2)

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(length one-through-four)

; Map

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(map abs (list -10 2.5 -11.6 17))

(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1) 
        (else (+ (count-leaves (car tree)) 
                 (count-leaves (cdr tree))))))

(define x (cons (list 1 2) (list 3 4)))
(count-leaves x)
(count-leaves (list 1 2 3 4))