#lang sicp

(define (enumerate low high)
  (if (> low high)
      nil
      (cons low (enumerate (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (filter predicate? sequence)
  (cond ((null? sequence) nil)
        ((predicate? (car sequence)) 
         (cons (car sequence) (filter predicate? (cdr sequence))))
        (else (filter predicate? (cdr sequence)))))

(define (flatmap f seqs)
  (accumulate append nil (map f seqs)))

(define (make-queen row col) (list row col))
(define (get-row queen) (car queen))
(define (get-col queen) (car (cdr queen)))

(define empty-board (list))
(define (adjoin-position row col queens) (cons (make-queen row col) queens))

(define (attack? q1 q2)
  (cond ((= (get-row q1) (get-row q2)) true)
        ((= (get-col q1) (get-col q2)) true)
        ((= (- (get-row q1) (get-col q1)) (- (get-row q2) (get-col q2))) true)
        ((= (+ (get-row q1) (get-col q1)) (+ (get-row q2) (get-col q2))) true)
        (else false)))

; the first queen is the queen that is added and should be
; compared to all other queens
(define (safe? col queens)
  (let ((new-queen (car queens))
        (rest-queens (cdr queens)))
    (= (length (filter (lambda(q) (attack? new-queen q)) rest-queens)) 0)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) 
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position 
                    new-row 
                    k 
                    rest-of-queens))
                 (enumerate
                  1 
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 4)
(length (queens 8))