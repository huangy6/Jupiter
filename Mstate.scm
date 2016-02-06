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

; takes a variable and state and returns the state with the vairable
; initialized to the empty list
(define Mstate_insert-var
    (lambda (variable state)
        (Mstate_construct (cons variable (Mstate_variables state))
                          (cons '() (Mstate_values state)))))

; takes a variable and state and returns true if variable is a member
; of the state, otherwise returns false
(define Mstate_contains-var?
    (lambda (variable state)
        (member variable (Mstate_variables state))))
