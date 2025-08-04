#lang sicp

(define (make-monitored f)
  (let ((call-count 0))
    (define (call x)
      (begin (set! call-count (+ call-count 1))
             (f x)))
    (define (how-many-calls?) call-count)
    (define (reset-count) (set! call-count 0))
    (lambda (arg)
      (cond ((eq? arg 'how-many-calls?) (how-many-calls?))
            ((eq? arg 'reset-count) (reset-count))
            (else (call arg))))))


(define s (make-monitored sqrt))

(s 100)
(s 64)

(s 'how-many-calls?)
(s 'reset-count)
(s 25)
(s 'how-many-calls?)
