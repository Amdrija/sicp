#lang sicp

(define (+ a b)
  (if (= a 0) 
      b 
      (inc (+ (dec a) b))))
; (+ 4 5) evaluates to
; (inc (+ 4 5))
;   (inc (+ 3 5))
;       (inc (+ 2 5))
;           (inc (+ 1 5))
;               (inc (+ 0 5)) -> 5
;           (inc 5) -> 6
;       (inc 6) -> 7
;   (inc 7) -> 8
; (inc 8) -> 9

(define (plus2 a b)
  (if (= a 0) 
      b 
      (plus2 (dec a) (inc b))))
; (plus2 4 5) evaluates to
; (plus2 3 6)
; (plus2 2 7)
; (plus2 1 8)
; (plus2 0 9) -> 9
