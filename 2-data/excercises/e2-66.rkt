#lang sicp

(define key-record car)
(define record-record cdr)

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (lookup key records)
  (cond ((null? records) nil)
        ((eq? key (key-record (entry records)) (record-record (entry records))))
        ((< key (key-record (entry records))) (lookup key (left-branch records)))
        (else (lookup key (right-branch records)))))
