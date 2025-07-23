#lang sicp

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list 
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list 
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry 
                                 left-tree 
                                 right-tree)
                      remaining-elts))))))))

(define (list->tree elements)
  (car (partial-tree 
        elements (length elements))))



(define (element-of-list? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-list? x (cdr set)))))

(define (adjoin-set x set) (cons x set))

(define (intersection-set s1 s2)
  (define (intersection-list l1 l2)
    (if (or (null? l1) (null? l2))
        '()
        (let ((x1 (car l1)) (x2 (car l2)))
          (cond ((= x1 x2) (cons x1 (intersection-list (cdr l1) (cdr l2))))
                ((< x1 x2) (intersection-list (cdr l1) l2))
                ((< x2 x1) (intersection-list l1 (cdr l2)))))))
  (list->tree (intersection-list (tree->list s1) (tree->list s2))))

(define (union-set s1 s2)
  (define (union-list l1 l2)
    (cond ((null? l1) l2)
          ((null? l2) l1)
          ((= (car l1) (car l2)) (cons (car l1) (union-list (cdr l1) (cdr l2))))
          ((< (car l1) (car l2)) (cons (car l1) (union-list (cdr l1) l2)))
          (else (cons (car l2) (union-list l1 (cdr l2))))))
  (list->tree (union-list (tree->list s1) (tree->list s2))))

(define s1 (list->tree (list 3 4 5 6)))
(define s2 (list->tree (list 1 2 3 4)))
(union-set s1 s2)
(intersection-set s1 s2)
