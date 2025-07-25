#lang sicp

; Each division needs to tag their file with a unique tag
; and has to install a record operation for that tag which
; takes the contents of the file and employee and returns
; the appropriate record.
(define (get-record file employee)
  (let ((division (get-division file)))
    (let ((record-operation (get-operation 'record division)))
      (apply-division division (record-operation (content-file) employee)))))


(define (get-salary record)
  ((get-operation 'salary (get-division record)) record))
    
(define (find-employee-record files employee)
  (car (filter (map files (lambda(file) (get-record file employee)))
               (lambda(record) (not (null? record))))))

; The new company just needs to tag their file with their division
; and install the function for salary and retrieveing an employee record
