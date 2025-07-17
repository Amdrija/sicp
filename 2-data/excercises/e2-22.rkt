#lang sicp

(define (square x) (* x x))

; Produces a reversed list because we always append to
; the start of the list
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

; This one on the other hand also doesnn't work
; as we'll create pairs in the form of
; (((nil, 1), 4), 9) ...
(define (square-list2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square 
                     (car things))))))
  (iter items nil))
