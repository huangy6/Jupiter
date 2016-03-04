; Created by Jack Mousseau, Vimig Socrates, and Yidi Huang
; Case Western Reserve PLC Spring 2016

; Statement Conditions

(define operator car)

(define var-declaration-stmt?
    (lambda (branch)
        (eq? 'var (operator branch))))

(define assigment-stmt?
    (lambda (branch)
        (eq? '= (operator branch))))

(define return-stmt?
    (lambda (branch)
        (eq? 'return (operator branch))))

(define if-stmt?
    (lambda (branch)
        (eq? 'if (operator branch))))

(define while-stmt?
    (lambda (branch)
        (eq? 'while (operator branch))))

(define break-stmt?
  (lambda (branch)
    (eq? 'break (operator branch))))

(define continue-stmt?
    (lambda (branch)
        (eq? 'continue (operator branch))))

(define stmt-block?
  (lambda (branch)
    (eq? 'begin (operator branch))))

(define throw-stmt?
  (lambda (branch)
    (eq? 'throw (operator branch))))

(define try-stmt?
  (lambda (branch)
    (eq? 'try (operator branch))))
