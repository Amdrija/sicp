#lang sicp

(define (f-rec n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        ((= n 2) 2)
        (else (+ (f-rec (- n 1))
                 (* 2 (f-rec (- n 2)))
                 (* 3 (f-rec (- n 3)))))))
(f-rec 0)
(f-rec 1)
(f-rec 2)
(f-rec 3)
(f-rec 4)

(define (f-it n)
  (define (f-iter current fn1 fn2 fn3)
    (if (= current n)
        fn3
        (f-iter (+ current 1)
                (+ fn1 (* 2 fn2) (* 3 fn3))
                fn1
                fn2)))
  (f-iter 0 2 1 0))

(f-it 0)
(f-it 1)
(f-it 2)
(f-it 3)
(f-it 4)

