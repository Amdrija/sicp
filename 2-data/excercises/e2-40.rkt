#lang sicp

(define (enumerate low high)
  (if (> low high)
      nil
      (cons low (enumerate (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap f seqs)
  (accumulate append nil (map f seqs)))

(flatmap (lambda(x) x) (list (list 1 2 3) (list 4) (list 5 6)))

(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda(j) (list i j))
                            (enumerate 1 (- i 1))))
           (enumerate 2 n)))

(unique-pairs 5)