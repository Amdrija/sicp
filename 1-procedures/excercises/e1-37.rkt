#lang sicp

(define (cont-frac n d k)
  (define (rec current)
    (if (> current k)
        0
        (/ (n current) (+ (d current) (rec (+ current 1))))))
  (rec 1))


(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           20)

(define (cont-frac-iter n d k)
  (define (iter result current)
    (if (= current 0)
        result
        (iter (/ (n current) (+ result (d current))) (- current 1))))
  (iter 0 k))

(cont-frac-iter (lambda (i) 1.0)
                (lambda (i) 1.0)
                20)