#lang sicp

(define (for-each f items)
  (if (null? items)
      true
      ((lambda (items)
          (f (car items)) 
          (for-each f (cdr items)))
       items)))
        
        

(for-each 
 (lambda (x) (newline) (display x))
 (list 57 321 88 2))