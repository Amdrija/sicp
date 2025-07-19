#lang sicp

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (subsets set)
  (if (null? set)
      (list nil)
      (let ((rest (subsets (cdr set)))
            (current (car set)))
        (append rest (map (lambda(s) (append s (list current))) rest)))))

(subsets (list 1 2 3))
