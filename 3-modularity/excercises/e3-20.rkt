#lang sicp
; When we do cons, it creates an environment E1 which binds x and y
; x is now bound to a procedure which references environemnt E1.
; When we call (define z (cons x x)). cons binds another environemnt E2
; where x and y are set to a procedure which references environment E1.
; z itself is a procedure which references environment E2.

; First we call (cdr z) which create an environment E3 -> global which binds z to z from global.
; This then calls z with 'cdr, which creates another environment E4 -> E2 -> global. It returns x.
; Then calling set-car on x will create another environment E5 -> global, whose z is set to x,
; which then calls x with 'set-car which creates another environment E6 -> E1 -> global. 
; This then calls 'set-car with x = 1 and v = 17 (creating another environment whose parent is E1).


(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined 
                 operation: CONS" m))))
  dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))

(define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)

(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)

(define x (cons 1 2))
; global <- E1: {x = 1, y = 2, set-x! ref E1, ..}
; x = lambda ref E1

(define z (cons x x))
; global <- E2: {x = lambda ref E1, y = lambda ref E1, set-x! ref E2, ...}
; z = lambda ref E2

(cdr z)
; global <- E3: {z = lambda ref E2}
; (z 'car)
; global <- E2 <- E4: {m = 'car}
; returns x = lamda ref E1


(set-car! (cdr z) 17)
; global <- E5: {z = lambda ref E1, new-value = 17}
; (z 'set-car!) <=> (x 'set-car)
; global <- E1 <- E6: {m = 'set-car!}
; return lambda ref E1
; (cons: set-car! 17)
; global <- E1 <- E7: {v = 17}
; this sets x.x = 17
(car x)