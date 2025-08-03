#lang sicp

(define (sub p1 p2)
  (define (add p1 (neg p2))))

(define (neg p)
  (make-poly (variable p) 
             (map (term-list p)
                  (lambda (term) (make-term (order term)
                                            (neg (coeff term)))))))
