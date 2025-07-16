#lang sicp

(define (pascal level n)
  (if (or (= level 0) (= n 0) (= n level))
      1
      (+ (pascal (- level 1) (- n 1)) (pascal (- level 1) n))))

(pascal 0 0)
(pascal 1 0)
(pascal 1 1)
(pascal 2 0)
(pascal 2 1)
(pascal 2 2)
(pascal 4 1)
(pascal 4 2)
