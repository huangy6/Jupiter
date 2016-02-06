; Created by Jack Mousseau, Vimig Socrates, and Yidi Huang
; Case Western Reserve PLC Spring 2016

; state structure is '((x y z ...) (3 5 7 ...))

(define Mstate_variables car)
(define Mstate_values cadr)

; takes a list of variables and a list of values and returns the state
; according to the structure defined at the top of this file
(define Mstate_construct
    (lambda (variables values)
        (append (cons variables '()) (cons values '()))))
