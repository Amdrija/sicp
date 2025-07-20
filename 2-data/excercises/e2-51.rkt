#lang sicp

; Vectors
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect v w) 
  (make-vect (+ (xcor-vect v) (xcor-vect w))
             (+ (ycor-vect v) (ycor-vect w))))

(define (scale-vect v c)
  (make-vect (* c (xcor-vect v)) (* c (ycor-vect v))))

(define (neg-vect v)
  (scale-vect v -1))

(define (sub-vect v w)
  (add-vect v (neg-vect w)))

(define (mid-point-vect v w)
  (scale-vect (add-vect v w) 0.5))

; Frames
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)

(define (corner-edge1-frame frame) (add-vect (origin-frame frame) (edge1-frame frame)))
(define (corner-edge2-frame frame) (add-vect (origin-frame frame) (edge2-frame frame)))
(define (corner-edge1edge2-frame frame) (add-vect (corner-edge1-frame frame) (edge2-frame frame)))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect 
      (scale-vect (xcor-vect v)
                  (edge1-frame frame))
      (scale-vect (ycor-vect v)
                  (edge2-frame frame))))))

; Painters
(define (transform-painter 
         painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame new-origin
                  (sub-vect (m corner1) 
                            new-origin)
                  (sub-vect (m corner2)
                            new-origin)))))))

(define (below p1 p2)
  (let ((split-point (make-vect 0 0.5)))
    (let ((paint-bottom (transform-painter p1
                                           (make-vect 0 0)
                                           (make-vect 1 0)
                                           split-point))
          (paint-top (transform-painter p1
                                        split-point
                                        (make-vect 1 0.5)
                                        (make-vect 0 1)))))
    (lambda(frame) (paint-top frame) (paint-bottom frame))))

(define (below2 p1 p2)
  (rot270 (beside (rot90 p2) (rot90 p1))))
                                          
