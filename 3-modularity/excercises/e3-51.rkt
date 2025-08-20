#lang sicp

(define (display-line x)
  (newline)
  (display x))
(define (show x)
  (display-line x)
  x)

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1)
                                  high))))

(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin 
        (proc (stream-car s))
        (stream-for-each proc 
                         (stream-cdr s)))))
(define (display-stream s)
  (stream-for-each display-line s))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream 
       (proc (stream-car s))
       (stream-map proc (stream-cdr s)))))


(define x 
  (stream-map 
   show 
   (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)