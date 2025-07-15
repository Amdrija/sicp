#lang sicp

(define (cont-frac n d k)
  (define (rec current)
    (if (> current k)
        0
        (/ (n current) (+ (d current) (rec (+ current 1))))))
  (rec 1))


(define (d i) (if (= (remainder i 3) 2)
                  (* 2.0 (/ (+ i 1) 3))
                  1.0))
(d 1)
(d 2)
(d 3)
(d 4)
(d 5)
(d 6)
(d 7)
(d 8)
(d 9)
(d 10)
(d 11)

(define (e-approx k)
  (+ 2 (cont-frac (lambda (i) 1.0)
                  d
                  30)))
(e-approx 10)
