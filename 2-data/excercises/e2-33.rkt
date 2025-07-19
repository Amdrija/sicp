#lang sicp

; Map
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

; Filter
(define (filter predicate? sequence)
  (cond ((null? sequence) nil)
        ((predicate? (car sequence)) 
         (cons (car sequence) (filter predicate? (cdr sequence))))
        (else (filter predicate? (cdr sequence)))))

; Accumulate / Reduce
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (map2 p sequence)
  (accumulate (lambda (element result) (append (cons (p element) result))) 
              nil 
              sequence))


(define (append2 seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda(element result) (+ result 1)) 0 sequence))


(map2 (lambda(x) (* x 2)) (list 1 2 3))
(append2 (list 4 5 6) (list 1 2 3))
(length (list 1 2 3 5))