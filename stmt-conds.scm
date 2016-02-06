; Created by Jack Mousseau + Vimig Socrates
; Case Western Reserve PLC Spring 2016

; Statement Conditions

(define var-declaration-stmt?
    (lambda (branch)
        (eq? 'var (car branch))))

(define assigment-stmt?
    (lambda (branch)
        (eq? '= (car branch))))

(define return-stmt?
    (lambda (branch)
        (eq? 'return (car branch))))

(define if-stmt?
    (lambda (branch)
        (eq? 'if (car branch))))

(define while-stmt?
    (lambda (branch)
        (eq? 'while (car branch))))
