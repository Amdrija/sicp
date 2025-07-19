#lang sicp

(define (enumerate low high)
  (if (> low high)
      nil
      (cons low (enumerate (+ low 1) high))))

(define (filter predicate? sequence)
  (cond ((null? sequence) nil)
        ((predicate? (car sequence)) 
         (cons (car sequence) (filter predicate? (cdr sequence))))
        (else (filter predicate? (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap f seqs)
  (accumulate append nil (map f seqs)))

(define (make-triples n)
  (flatmap (lambda(i) (flatmap (lambda(j) (map (lambda(k) (list i j k))
                                               (enumerate 1 (- j 1))))
                               (enumerate 1 (- i 1))))
           (enumerate 3 n)))

(define (triple-sum triple)
  (+ (car triple) (car (cdr triple)) (car (cdr (cdr triple)))))

(define (find-triples n sum)
  (filter (lambda (triple) (= (triple-sum triple) sum))
          (make-triples n)))

(find-triples 8 10)
