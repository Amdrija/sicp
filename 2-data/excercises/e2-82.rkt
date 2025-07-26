; 1. You cannot coerce all arguments to the same type, as there
; may be an operation that can take mixed types. For example
; suppose there's an operation which takes a real number and a complex
; number and you try to call it with an integer and a rational number.
; If you try to coerce all to the same type, it won't work as it will try
; to coerce all to integer or all to rational and will never find this operation

; How to implement this:
; 1. Define a function that takes a type and a list of arguments and coerces 
; all arguments to this type - either returns the mapped list or nil (if there's a 
; missing coercion)
; 2. Then, try to find the operation with the list of arguments of the type
; 3. If there's an operation, apply it and return
; 4. Otherwise, call the procedure with the next type in the argument list

(define (all-convertable type coerced-args)
  (or (null? args)
      (and (get-coercion type (type-tag (car args)))
           (all-convertable type (cdr args)))))

(define (coerce-list type args)
  (map args (lambda(arg) ((get-coercion type (car arg)) (contents arg)))))

(define (apply-generic op . args)
  (define (coerce-type args-left)
    (let ((current-type (type-tag (car tags)))
          (proc (get-proc op (map (args (lambda (x) current-type))))))
      (cond ((null? args-left) (error "No method for types"))
            ((and (all-convertable current-type args) proc)
             (apply proc (coerce-list current-type args)))
            (else (coerce-type (cdr args-left))))))
  (let ((proc (get op (map args type-tag)))))
    (if proc
        (apply proc (map args contents))
        (coerce-type args)))