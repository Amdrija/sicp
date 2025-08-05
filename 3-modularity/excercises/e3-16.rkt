#lang sicp

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(count-pairs (list 1 2 3)); [1 | * ] -> [2 | *] -> [3 | *] -> nil
;Returns 3

(define x (list 1)); [1 | *] -> nil

; Returns 4
(count-pairs (cons x (cons x 1)))
; [* | *] --->[* | 1]
;  |           |
;  |           |   
;  |           |
;  |----------[1 | *] -> nil

(define xx (cons x x))
; [* | *] ---->|
;  |           v
;  |----------[1 | *] -> nil

(count-pairs (cons xx xx))
; [* | *] ---->|
;  |           v
;  |--------->[* | *] ---->|
;              |           v
;              |----------[1 | *] -> nil
; Returns 7

(define cycle (list 1))
(set-cdr! cycle cycle)
(count-pairs cycle)
; [1 | *] ---|
;  |         |
;  |---------|