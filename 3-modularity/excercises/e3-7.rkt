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
  (define (dispatch pass m)
    (cond ((not (eq? pass password)) (lambda(x) "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: 
                 MAKE-ACCOUNT" m))))
  dispatch)

(define (make-joint account password new-password)
  (lambda (pass op)
    (if (eq? pass new-password)
        (account password op)
        (error "Incorrect password"))))

(define acc 
  (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)

((acc 'some-other-password 'deposit) 50)

(define paul-acc
  (make-joint acc 
              'secret-password 
              'rosebud))
((paul-acc 'rosebud 'deposit) 100)
((acc 'secret-password 'withdraw) 50)

