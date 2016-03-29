;; Created by Jack Mousseau, Vimig Socrates, and Yidi Huang
;; Case Western Reserve PLC Spring 2016

;; Assumes expressions of form (operator op1 op2 ...) consisting of 
;; an operator and at least one operand

(define operator car)

; Arity testers 
(define unary? (lambda (exp) (null? (cddr exp))))
(define binary? (lambda (exp) (and (not (unary? exp)) (null? (cdddr exp)))))
; (define ternary? (lambda (exp) (null? (cddddr exp))))

; Unary operators
(define math_neg-expression? (lambda (exp) (and (unary? exp) (eq? (operator exp) '-))))
(define bool_neg-expression? (lambda (exp) (and (unary? exp) (eq? (operator exp) '!))))

; Binary operators
(define add-expression? (lambda (exp) (and (binary? exp) (eq? (operator exp) '+))))
(define sub-expression? (lambda (exp) (and (binary? exp) (eq? (operator exp) '-))))
(define mult-expression? (lambda (exp) (and (binary? exp) (eq? (operator exp) '*))))
(define div-expression? (lambda (exp) (and (binary? exp) (eq? (operator exp) '/))))
(define mod-expression? (lambda (exp) (and (binary? exp) (eq? (operator exp) '%))))
(define eq?-expression? (lambda (exp) (and (binary? exp) (eq? (operator exp) '==))))
(define noteq?-expression? (lambda (exp) (and (binary? exp) (eq? (operator exp) '!=))))
(define lt?-expression? (lambda (exp) (and (binary? exp) (eq? (operator exp) '<))))
(define gt?-expression? (lambda (exp) (and (binary? exp) (eq? (operator exp) '>))))
(define lteq?-expression? (lambda (exp) (and (binary? exp) (eq? (operator exp) '<=))))
(define gteq?-expression? (lambda (exp) (and (binary? exp) (eq? (operator exp) '>=))))
(define bool_and-expression? (lambda (exp) (and (binary? exp) (eq? (operator exp) '&&))))
(define bool_or-expression? (lambda (exp) (and (binary? exp) (eq? (operator exp) '||))))

; Non-ary operators
(define funcall-expression? (lambda (exp) (eq? (operator exp) 'funcall)))