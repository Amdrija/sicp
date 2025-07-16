#lang sicp

(define (make-interval a b) (cons a b))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

(define (make-center-percent c p)
  (let ((factor (/ p 100.0)))
    (make-interval (* c (- 1 factor)) (* c (+ 1 factor)))))

(define (center i)
  (/ (+ (lower-bound i) 
        (upper-bound i)) 
     2))

(define (width i)
  (/ (- (upper-bound i) 
        (lower-bound i)) 
     2))

(define (percent i)
  (* 100.0 (/ (width i) (center i))))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (<= (lower-bound y) 0 (upper-bound y))
      (error 'div-interval "Cannot divide by an interval that spans 0")
      (mul-interval x 
                    (make-interval 
                      (/ 1.0 (upper-bound y)) 
                      (/ 1.0 (lower-bound y))))))

(define (sub-interval x y)
  (add-interval x
                (make-interval (- (upper-bound y)) (- (lower-bound y)))))


(define (print-interval i)
  (display "[")
  (display (lower-bound i))
  (display ", ")
  (display (upper-bound i))
  (display "]")
  (newline))

(define r1 (make-interval 6.12 7.48))
(define r2 (make-interval (* 4.7 0.95) (* 4.7 1.05)))

(define one (make-interval 1.0 1.0))
(define para (div-interval one
                           (add-interval (div-interval one r1)
                                         (div-interval one r2))))
(define (parallel-1 r1 r2)
  (div-interval (mul-interval r1 r2) (add-interval r1 r2)))

(define (parallel-2 r1 r2)
  (div-interval one (add-interval (div-interval one r1)
                                  (div-interval one r2))))

(percent (parallel-1 r1 r2))
(percent (parallel-2 r1 r2))

(center (div-interval r1 r1))
(percent (div-interval r1 r1))

(center (div-interval r2 r2))
(percent (div-interval r2 r2))
