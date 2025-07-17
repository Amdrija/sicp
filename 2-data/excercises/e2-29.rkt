#lang sicp

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch m) (car m))
(define (right-branch m) (car (cdr m)))

(define (branch-length b) (car b))
(define (branch-structure b) (car (cdr b)))
(define (mobile? m) (pair? m))

(define (branch-weight b)
  (let ((structure (branch-structure b)))
    (if (mobile? structure)
        (+ (branch-weight (left-branch structure)) (branch-weight (right-branch structure)))
        structure)))
(define (total-weight m)
  (+ (branch-weight (left-branch m)) (branch-weight (right-branch m))))

(define m1 (make-mobile (make-branch 2 5) (make-branch 1 7)))
(total-weight m1)

(define m2 (make-mobile (make-branch 1 3) (make-branch 2 m1)))
(total-weight m2)

(define m3 (make-mobile (make-branch 3 m1) (make-branch 4 m2)))
(total-weight m3)


(define (branch-torque b) (* (branch-length b) (branch-weight b)))

(define (balanced? m)
  (let ((left-structure (branch-structure (left-branch m)))
        (right-structure (branch-structure (right-branch m))))
    (and (= (branch-torque (left-branch m)) (branch-torque (right-branch m)))
         (and (or (not (mobile? left-structure)) 
                  (balanced? left-structure)) 
              (or (not (mobile? right-structure))
                  (balanced? right-structure))))))

(balanced? m1)
(balanced? m2)
(balanced? m3)

(define b1 (make-mobile (make-branch 2 6) (make-branch 6 2)))
(balanced? b1)

(define b2 (make-mobile (make-branch 8 24) (make-branch 24 b1)))
(balanced? b2)
(total-weight b2)

(define b3 (make-mobile (make-branch 16 1) (make-branch 1 (make-mobile (make-branch 2 8) (make-branch 2 8)))))
(balanced? b3)
(total-weight b3)

(define b4 (make-mobile (make-branch 17 b2) (make-branch 32 b3)))
(balanced? b4)
