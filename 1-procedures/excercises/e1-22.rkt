#lang sicp

(define (prime? n)
  (define (prime-iter n factor)
    (cond ((> (* factor factor) n) true)
          ((= (remainder n factor) 0) false)
          (else (prime-iter n (+ factor 1)))))
  (prime-iter n 2))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) 
                         start-time))
      false))

(define (report-prime n elapsed-time)
  (display "Prime=")
  (display n)
  (display " time: ")
  (display elapsed-time)
  (display "us")
  (newline)
  true)

(define (search-for-prime start end)
  (define (iter current count)
   (cond ((= current end) (newline) (display "not found 3"))
         ((= count 3) (newline) (display "found 3"))
         (else (if (timed-prime-test current)
                   (iter (+ current 1) (+ count 1))
                   (iter (+ current 1) count)))))
  (iter start 0))

(search-for-prime 100000000 1000000000)
(search-for-prime 1000000000 10000000000)
(search-for-prime 10000000000 100000000000)