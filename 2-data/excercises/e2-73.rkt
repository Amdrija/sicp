#lang sicp

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (install-sum)
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) 
           (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (deriv-sum sum var)
    (list '+ (make-sum (deriv (car sum) var) (deriv (cdr sum) var))))
  (put 'deriv '+ deriv-sum)
  (put 'make-sum '+ make-sum))

(define (install-product)
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) 
                (=number? m2 0)) 
            0)
            ((=number? m1 1) m2)
            ((=number? m2 1) m1)
            ((and (number? m1) (number? m2)) 
            (* m1 m2))
            (else (list '* m1 m2))))
  (define (deriv-product p var)
    (let ((make-sum (get 'make-sum '+)))
      (make-sum (make-product (deriv (car p) var) (cdr p))
                (make-product (car p) (deriv (cdr p) var)))))
  (put 'deriv '* deriv-product)
  (put 'make-product '* make-product))

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) 
           (if (same-variable? exp var) 
               1 
               0))
         (else ((get 'deriv (operator exp)) 
                (operands exp) 
                var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(install-sum)
(install-product)

; 1. We can't assimilate the predicate number? and varianble?
; because they are not pairs

; This is just a virtual table thats it