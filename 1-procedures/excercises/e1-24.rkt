#lang sicp

(define (even? x) (= (remainder x 2) 0))
(define (square x) (* x x))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m))
                                m))
        (else (remainder (* (expmod base (- exp 1) m) base)
                         m))))        

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) 
         (fast-prime? n (- times 1)))
        (else false)))

(define (prime? n) (fast-prime? n 100))

(define (time f arg)
  (define start (runtime))
  (f arg)
  (display "Time: ")
  (display (- (runtime) start))
  (display "us")
  (newline))

(time prime? 100000007)
(time prime? 1000000007)