#lang sicp

(define (make-from-mag-ang magnitute angle)
  (lambda (op)
    (cond ((eq? op 'real-part) (* magnitute (cos angle)))
          ((eq? op 'imag-part) (* magnitude (sin angle)))
          ((eq? op 'magnitude) magnitude)
          ((eq? op 'angle) angle)
          (else (error "unknon operation")))))
