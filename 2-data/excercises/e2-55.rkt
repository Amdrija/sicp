#lang sicp

(car ''abracadabra) 
; Prints the quote as the interpreter treats the charaters after ' as (quote <>)
; So this evaluates to (quote (quote abracadabra))