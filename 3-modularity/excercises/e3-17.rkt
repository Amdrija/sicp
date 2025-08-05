#lang sicp

(define (count-pairs x)
  (define seen (list))
  (define (iter x)
    (if (or (not (pair? x)) (memq x seen))
        0
        (begin (set! seen (cons x seen))
               (+ (iter (car x))
                  (iter (cdr x))
                  1))))
  (iter x))

(count-pairs (list 1 2 3)); [1 | * ] -> [2 | *] -> [3 | *] -> nil

(define x (list 1)); [1 | *] -> nil

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

(define cycle (list 1))
(set-cdr! cycle cycle)
(count-pairs cycle)
; [1 | *] ---|
;  |         |
;  |---------|
