#lang sicp

; recursive factorial
(define (factorial n) 
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(factorial 5)

; iterative facotiral
(define (fact n)
  (define (fact-iter product current)
    (if (> current n)
        product
        (fact-iter (* product current) (+ current 1))))
  (fact-iter 1 1))

(fact 5)

; fibonacci
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) 
                 (fib (- n 2))))))
(fib 6)

(define (fib2 n)
  (define (fib-iter current current_fib last_fib)
    (if (= current n)
        last_fib
        (fib-iter (+ current 1) (+ current_fib last_fib) current_fib)))
  (fib-iter 0 1 0))

(fib2 6)

; counting change

(define (change-count total)
  (define (coin-total coin)
    (cond ((= coin 1) 1) 
          ((= coin 2) 5)
          ((= coin 3) 10)
          ((= coin 4) 25)
          ((= coin 5) 50)))
  (define (change-count-iter total coins)
    (cond ((= total 0) 1)
          ((< total 0) 0)
          ((= coins 0) 0)
          (else (+ (change-count-iter (- total (coin-total coins)) coins)
                   (change-count-iter total (- coins 1))))))
  (change-count-iter total 5))

(change-count 100)

; exponentiation

(define (exp-rec x n)
  (define (square a) (* a a))
  (cond ((= n 0) 1)
        ((= n 1) x)
        ((= (remainder n 2) 0) (square (exp-rec x (/ n 2))))
        (else (* x (exp-rec x (- n 1))))))
        
(exp-rec 100 0)
(exp-rec 25 1)
(exp-rec 3 3)
(exp-rec 2 10)
(exp-rec 2 4)
(exp-rec 2 7)

; GCD
(define (gcd a b)
  (if (= b 0) 
      a
      (gcd b (remainder a b))))

(gcd 206 40)
(gcd 36 42)

; primes
(define (prime? n)
  (define (prime-iter n factor)
    (cond ((> (* factor factor) n) true)
          ((= (remainder n factor) 0) false)
          (else (prime-iter n (+ factor 1)))))
  (prime-iter n 2))

(prime? 5)
(prime? 9)
(prime? 720)
(prime? 197)
        
