#lang sicp

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((wrong-attempts 7))
    (define (dispatch pass m)
      (if (eq? pass password)
          (begin
            (set! wrong-attempts 7)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  (else (error "Unknown request: 
                                MAKE-ACCOUNT" m))))
          (if (= wrong-attempts 0)
              (lambda (x) "call the cops")
              (begin (set! wrong-attempts (- wrong-attempts 1))
                     (lambda(x) "Incorrect password")))))
    dispatch))

(define acc 
  (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)

((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50) ; This call should call the cops