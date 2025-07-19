#lang sicp

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (reverse-1 sequence)
  (fold-right (lambda (element result) (append result (list element))) nil sequence))

(define (reverse-2 sequence)
  (fold-left (lambda (result element) (cons element result)) nil sequence))

(reverse-1 (list 1 2 3))
(reverse-2 (list 1 2 3))
